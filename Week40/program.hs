import Language.Haskell.TH (valD, safe)


{- AST -}

data StringExpression a = SVar a
                        | Literal String
                        | Concat (StringExpression a) (StringExpression a)
                        | UserString
    deriving (Eq,Show)

data BoolExpression a = T | F
                    | StringEq (StringExpression a) (StringExpression a)
    deriving (Eq,Show)

data Statement a = Print (StringExpression a)
                 | If (BoolExpression a) (Statement a)
                 | StrAssign a (StringExpression a)
    deriving (Eq,Show)

{- Evaluate and execute ASTs -}

evalString :: (a -> String) -> StringExpression a -> IO String
evalString f (SVar v) = pure (f v)
evalString f (Literal s) = pure s
evalString f (Concat lhs rhs) = do
    lval <- evalString f lhs
    hval <- evalString f rhs
    pure (lval ++ hval)
evalString f UserString = getLine

evalBool :: (a -> String) -> BoolExpression a -> IO Bool
evalBool f T = pure True
evalBool f F = pure False
evalBool f (StringEq lhs rhs) = do
    lval <- evalString f lhs
    rval <- evalString f rhs
    pure (lval == rval)

runStatement :: (Eq a) => (a -> String) -> Statement a -> IO (a -> String)
runStatement f  (Print s) = do
    sval <- evalString f s
    putStrLn sval 
    pure f
runStatement f (If b statement) = do
    bval <- evalBool f b
    if bval then runStatement f statement
    else pure f
runStatement f (StrAssign var expr) = do
    val <- evalString f expr
    pure (\x -> if x == var then val else f x)

type Parser e = String -> Maybe (String, e)

keyWord :: String -> Parser ()
keyWord [] s = Just (s, ())
keyWord (k:ks) [] = Nothing
keyWord (k:ks) (s:ss) = if k == s then keyWord ks ss else Nothing  

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \s -> case p s of 
                Nothing -> q s
                (Just v) -> Just v

pUserString, pSVar, pConcat, pLiteral :: Parser (StringExpression String)

pUserString s = do
    (s',_) <- keyWord "read" s
    Just (s', UserString)

alpha x = elem x ['a'..'z']

pSVar s = let v = takeWhile alpha s
              s' = dropWhile alpha s
    in if v == "" then Nothing else Just (s',SVar v)

pConcat s = do
    (s,lhs) <- (pUserString <|> pSVar <|> pConcat <|> pLiteral ) s
    (s,_) <- keyWord "+" s
    (s,rhs) <- parseString s
    Just (s, Concat lhs rhs)

allowed x = elem x $ ['a'..'z'] ++ ['A'..'Z'] ++ " !?.,"

pChars s = Just (dropWhile allowed s, takeWhile allowed s)

pLiteral s = do
    (s,_) <- keyWord "\"" s
    (s,lit) <- pChars s
    (s,_) <- keyWord "\"" s
    Just (s, Literal lit)


parseString :: Parser (StringExpression String)
parseString = pUserString <|> pSVar <|> pConcat <|> pLiteral 

pT s = do
    (s,_) <- keyWord "true" s
    Just (s, T)

pF :: String -> Maybe (String, BoolExpression a)
pF s = do
    (s,_) <- keyWord "false" s
    Just (s, F)

pStrEq s = do
    (s,lhs) <- parseString s
    (s,_) <- keyWord "=" s
    (s,rhs) <- parseString s
    Just (s, StringEq lhs rhs)

pSpace (' ':s) = Just (s,())
pSpace x = Nothing

pPrint s = do
    (s,_) <- keyWord "Print" s
    (s,_) <- pSpace s
    (s,expr) <- parseString s
    Just (s, Print expr)

pIf s = do 
    (s,_) <- keyWord "if" s
    (s,_) <- pSpace s
    (s,b) <- parseBool s
    (s,_) <- pSpace s
    (s,_) <- keyWord "Then" s
    (s,_) <- pSpace s
    (s,stm) <- parseStatement s
    Just (s, If b stm)

varname s = let v = takeWhile alpha s
                s' = dropWhile alpha s
            in if v == "" then Nothing else Just (s', v)    

pAssign s = do
    (s,x) <- varname s
    (s,_) <- keyWord "=" s
    (s,expr) <- parseString s
    Just (s, StrAssign x expr)
parseStatement :: Parser (Statement String)
parseStatement = pPrint <|> pIf <|> pAssign

parseBool :: Parser (BoolExpression String)
parseBool = pT <|> pF <|> pStrEq