import Control.Applicative

funs = [(++"bar"),init,("foo"++)]
str1 = ["123","abc"]
appl1 = funs <*> str1

conStr = liftA2 (++) "abc" "123" 