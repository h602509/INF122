{-# LANGUAGE TupleSections,TypeSynonymInstances,FlexibleInstances,TypeFamilies #-}
module Oblig1 where
import Control.Arrow ((***))
import Control.Monad
import Data.List (intersperse,transpose,genericLength)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (reads)
import Control.Applicative
import Control.Monad.State
import System.IO
import System.Exit
import Data.Time.Format.ISO8601 (yearFormat)
import Data.Maybe (fromMaybe)


-- | A data type to represent expressions that can be evaluated to a number.
-- This type is parametric over both the number type and the cell reference type.
data Expression number cell
  = Ref cell                     -- ^ Reference to another cell
  | Constant number              -- ^ Constant numerical value
  | Sum (CellRange cell)         -- ^ Summation over a range of cells
  | Add
      (Expression number cell)   -- ^ Left operand of addition
      (Expression number cell)   -- ^ Right operand of addition
  | Mul
      (Expression number cell)   -- ^ Left operand of multiplication
      (Expression number cell)   -- ^ Right operand of multiplication
  deriving (Eq, Ord)

instance (Show number, Show cell) => Show (Expression number cell) where
  show (Ref cell) = "!" ++ show cell
  show (Constant n) = show n
  show (Sum (Box ul lr)) = "SUM(" ++ show ul ++ ":" ++ show lr ++ ")"
  show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (Mul a b) = "(" ++ show a ++ "*" ++ show b ++ ")"

data CellRange cell = Box { upperLeft  :: cell
                          , lowerRight :: cell}
    deriving (Eq, Ord, Show)

-- | The ranged class gives a method of indexing ranges of cells
--   in the spreadsheet
class (Ord cell,Show cell) => Ranged cell where
    data Dimension cell
    cellRange :: Dimension cell -> CellRange cell -> Set cell

-- | A data type representing a sheet. 
-- It consists of a name and a mapping from cell references to expressions.
data Sheet number cell = Sheet
  { name :: String,
    -- ^ The name of the sheet
    dimension :: Dimension cell,
    -- ^ The dimension of the sheet
    content :: Map cell (Expression number cell)
    -- ^ The content of the sheet as a mapping from cell references to expressions
  }

-- hjelpefunksjon for å hente ut content av en celle
cellLookup :: Ord cell => Sheet number cell -> cell -> Maybe (Expression number cell)
cellLookup sheet cRef = Map.lookup cRef (content sheet)

-- | CellRef is the standard way to refer to a cell in the spreadsheet.
data CellRef = Cell { column :: Char, row :: Integer }
  deriving (Eq,Ord)

instance Show CellRef where
    show (Cell c r) = c:show r

instance Ranged CellRef where
    data Dimension CellRef
       = Dimension { columns :: [Char]
                   , rows :: [Integer] }
    cellRange dim ran = Set.intersection sheetSet rangeSet
      where 
        sheetSet = Set.fromList [Cell c r | c <- columns dim, r <- rows dim]
        rangeSet = Set.fromList [Cell c r | 
          c <- [column (upperLeft ran) .. column (lowerRight ran)], 
          r <- [row (upperLeft ran) .. row (lowerRight ran)]]

-- | A sample spreadsheet using Double for numeric type
sheet1 :: Sheet Double CellRef
sheet1 =
  Sheet
    { name = "Sheet1", -- Name of the sheet
      dimension = Dimension "ABC" [1..3],
      content = 
        Map.fromList
          [ ((Cell 'A' 1), Constant 12),
            ((Cell 'B' 1), Mul (Ref (Cell 'A' 1)) (Ref (Cell 'A' 2))),
            ((Cell 'C' 1), Ref (Cell 'C' 3)),
            ((Cell 'A' 2), Constant 4),
            ((Cell 'B' 2), Add (Ref (Cell 'B' 1)) (Ref (Cell 'A' 2))),
            ((Cell 'C' 2), Constant 0),
            ((Cell 'A' 3), Constant 9),
            ( (Cell 'B' 3),
              Sum (Box (Cell 'A' 1) (Cell 'B' 2))
            ),
            ((Cell 'C' 3), Constant 0)
          ]
    }

sheet2 :: Sheet Double CellRef
sheet2 =
  Sheet
    { name = "Sheet2",
      dimension = Dimension "ABC" [1..2],
      content =
        Map.fromList
          [ ((Cell 'A' 1), Constant 12),
            ((Cell 'B' 1), Mul (Constant 4) (Ref (Cell 'A' 2))),
            ((Cell 'C' 1), Add (Ref (Cell 'A' 1)) (Ref (Cell 'C' 2))),
            ((Cell 'A' 2), Constant 2),
            ((Cell 'B' 2), Constant 4),
            ((Cell 'C' 2), Sum (Box (Cell 'A' 1) (Cell 'C' 1)))
          ]
    }

-- Evaluate an expression within the context of a sheet.
-- Return Nothing if the expression cannot be evaluated.
evaluate :: (Num number, Ranged cell)
         => Sheet number cell
         -> Expression number cell
         -> Maybe number

-- Hvis uttrykket er en referanse til en annen cell vil vi evaluere den cellen.
evaluate sheet (Ref c1) = if hasCycle sheet c1 [] then Nothing else evaluateMaybe refCell
  where 
    refCell = Map.lookup c1 (content sheet)

    --hjelpefunksjon for å håndtere Maybe
    evaluateMaybe Nothing = Nothing
    evaluateMaybe (Just c2) = evaluate sheet c2

-- Hvis uttrykket er et konstant tall skal vi returnere det tallet. 
evaluate sheet (Constant n) = Just n 

-- Hvis uttrykket er en sum av celler skal vi rekursivt evaluere alle cellene og så summere resultatene.
evaluate sheet (Sum ran) = if cycle then Nothing else sumRange sheet list
  where
    refs = Set.toList $ cellRange (dimension sheet) ran
    list = map (cellLookup sheet) refs
    cycle = any hasCycle' refs
    hasCycle' cell = hasCycle sheet cell []

    -- Bruker do notasjonen til Maybe Monade for å håndtere Maybe
    sumRange sheet (x:xs) = do
      expr <- x 
      value <- evaluate sheet expr
      valueSum <- sumRange sheet xs
      return (value + valueSum)
    sumRange sheet [] = Just 0

-- Hvis uttrykket er addisjon av to uttrykk skal vi rekursivt evaluere enhvert uttrykk og så addere resultatene.
evaluate sheet (Add x y) = do
  x' <- evaluate sheet x
  y' <- evaluate sheet y
  return (x' + y') 

-- Hvis uttrykket er multiplikasjon av to uttrykk skal vi rekursivt evaluere enhvert uttrykk og så multiplisere resultatene.
evaluate sheet (Mul x y) = do
  x' <- evaluate sheet x
  y' <- evaluate sheet y
  return (x' * y') 

hasCycle :: (Ranged cell) => Sheet number cell -> cell -> [cell] -> Bool
hasCycle sheet cell visited = if cycleFound 
  then True 
  else fromMaybe False $ do
    expr <- cellLookup sheet cell  
    let refs = Set.toList(getExprRefs expr (dimension sheet))
    return (any hasCycle' refs)

  where 
    hasCycle' cell' = hasCycle sheet cell' (cell : visited)
    cycleFound = elem cell visited

-- hjelpefunksjon for å hente ut alle referanser i et expression
getExprRefs :: (Ord cell, Ranged cell) => Expression number cell -> Dimension cell -> Set cell
getExprRefs (Ref cell) dim = Set.singleton cell 
getExprRefs (Constant number) dim = Set.empty
getExprRefs (Sum ran) dim = cellRange dim ran
getExprRefs (Add l r) dim = Set.union (getExprRefs l dim) (getExprRefs r dim)
getExprRefs (Mul l r) dim = Set.union (getExprRefs l dim) (getExprRefs r dim)

-- The type of parsers
newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

-- Functor instance for Parser
instance Functor Parser where
  fmap f p = Parser (fmap (id *** f) . runParser p)

-- Applicative instance for Parser
instance Applicative Parser where
  pure x = Parser (\s -> Just (s, x))
  f <*> x = Parser $ \s -> do
    (s', f') <- runParser f s
    (s'', x') <- runParser x s'
    return (s'', f' x')

-- Monad instance for Parser
instance Monad Parser where
  return = pure
  p >>= f =
    Parser
      ( \s -> do
          (s', x) <- runParser p s
          runParser (f x) s'
      )

-- Alternative instance for Parser
instance Alternative Parser where
  empty = Parser $ const Nothing
  p <|> q = Parser $ \s -> runParser p s <|> runParser q s


-- A set of utility parsers

-- | Parse a single character
pChar :: Parser Char
pChar = Parser pHead where
   pHead "" = Nothing
   pHead (c:cs) = Just (cs , c)


exactChar :: Char -> Parser ()
exactChar char = do
  c <- pChar
  guard (c == char)

-- | Eat a single space
pSpace :: Parser ()
pSpace = do
   c <- pChar
   guard (c == ' ')

-- | Eat a single newline
pNewLine :: Parser ()
pNewLine = do
   c <- pChar
   guard (c == '\n')
   
-- | Parse a keyword
keyword :: String -> Parser ()
keyword [] = return ()
keyword (k : ks) = do
    c <- pChar
    guard (c == k)
    keyword ks

   
between :: Parser a -> Parser b -> Parser c -> Parser c
between pOpen pClose pContent = do
    pOpen
    x <- pContent
    pClose
    return x

-- | Parse parenthesis
inParenthesis :: Parser a -> Parser a
inParenthesis p = do
    keyword "("
    x <- p
    keyword ")"
    return x

-- | Parse brackets
inBrackets p = do
    keyword "["
    x <- p
    keyword "]"
    return x

-- | Parse an operator
pOperator :: String -> (t -> t -> t) -> Parser t -> Parser t
pOperator symbol constructor pOperand = do
  a <- pOperand
  rest a
  where
    rest a = (do
      keyword symbol
      b <- pOperand
      rest (constructor a b)) <|> return a

-- | Convert a Read instance to a parser
pRead :: (Read a) => Parser a
pRead =
  Parser
    ( \s -> case reads s of
        [] -> Nothing
        ((v, s) : _) -> Just (s, v)
    )

-- | Parse cell expressions
pExpression :: (Read number)
            => Parser (Expression number CellRef)
pExpression = undefined

-- | Parse an atomic term
pTerm :: Read number => Parser (Expression number CellRef)
pTerm =  inParenthesis pExpression
     <|> pConstant
     <|> pRef
     <|> pSum

-- | Parse a numeric constant
pConstant :: (Read number) => Parser (Expression number cell)
pConstant = Constant <$> pRead

-- | Parse a cell name
pCell :: Parser CellRef
pCell = do
  c <- pColName
  n <- pRowNumber
  return (Cell c n)

-- | Parse a cell reference
pRef :: Parser (Expression number CellRef)
pRef = do
  c <- exactChar '!'
  r <- pCell
  return (Ref r)


-- | Parse a multiplication expression
pMul :: Read number => Parser (Expression number CellRef)
pMul = undefined

-- | Parse an addition expression
pAdd :: Read number => Parser (Expression number CellRef)
pAdd = undefined


-- | Parse a sum of cell refences like SUM(A1:C3)
pSum :: Parser (Expression number CellRef)
pSum = undefined

-- Now follows parsers for the sheet structure itself

alphanum :: Char -> Bool
alphanum x = elem x (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])

alpha :: Char -> Bool
alpha x = elem x ['A' .. 'Z']

num :: Char -> Bool
num x = elem x (['0' .. '9'])

-- | Parse a row number
pRowNumber :: Parser Integer
pRowNumber = Parser (\s -> Just (dropWhile num s, read (takeWhile num s) :: Integer))

-- | Parse a column name
pColName :: Parser Char
pColName = do
  c <- pChar
  guard (alpha c)
  return c

-- | Parse a sheet name
pSheetName :: Parser String
pSheetName = Parser (\s -> Just (dropWhile alphanum s, takeWhile alphanum s))

-- | Parse a row, starting with "[n]" indicating the row number
pRow :: Read number => String -> Parser (Integer,[(CellRef, Expression number CellRef)])
pRow cols = do
   row <- inBrackets pRowNumber
   many pSpace
   cells <- many (inBrackets pExpression <* many pSpace)
   return (row,[ ((Cell col row),ce) | (ce , col) <- zip cells cols ])


-- | Parse a spreadsheet
pSheet :: (Read number) => Parser (Sheet number CellRef)
pSheet = do
   name <- inBrackets pSheetName
   many pSpace
   cols <- many (inBrackets pColName <* many pSpace)
   pNewLine
   rows <- many (pRow cols <* many pSpace <* pNewLine)
   let dim = Dimension cols (fst <$> rows)
   return (Sheet name dim (Map.fromList (concat $ snd <$> rows)))

-- | Utility function to pad a list of columns to
--   specified lengths
padColumns :: [Integer] -> [String] -> String
padColumns lengths columns = concat $ intersperse " " $ zipWith pad lengths columns where
    pad len str = zipWith const (str ++ repeat ' ') [0..len -1]

-- | Pretty print a spreadsheet
instance (Show number) => Show (Sheet number CellRef) where
 show sheet = unlines (padColumns maxWidths <$> printedRows) where
    bracket s = "[" ++ s ++ "]"
    printedRows = (bracket (name sheet) : ( (bracket . pure) <$> columns (dimension sheet)))
                : [bracket (show r) : [maybe "" (bracket . show)
                                                (Map.lookup (Cell c r)
                                                            (content sheet))
                                         | c <- columns (dimension sheet)]
                              | r <- rows (dimension sheet)]
    maxWidths = (maximum . map genericLength) <$> transpose printedRows
                       
--  | Read a spreadsheet from file, evaluate and print it
getSpreadSheet :: FilePath -> IO (Sheet Double CellRef)
getSpreadSheet file = do
   unparsed <- readFile file
   case runParser pSheet unparsed of
      Nothing -> do 
                  hPutStrLn stderr "No spreadsheet found"
                  exitWith (ExitFailure 1)
      (Just (_, sheet)) -> return sheet

--  | Read a spreadsheet from file, evaluate and print it
runSpreadSheet :: FilePath -> IO (Sheet Double CellRef)
runSpreadSheet file = do
    sheet <- getSpreadSheet file
    let evaluated = Map.mapMaybe ((Constant <$>) . evaluate sheet)
                                 (content sheet)
    return $ Sheet (name sheet)
                   (dimension sheet)
                   evaluated

