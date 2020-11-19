{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.Proxy
import SyntaxOptics
import VerboseOptics

data Expr
    = Lit Int
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Show, Eq)
makePrisms ''Expr

expr :: VerbosePrism' String String Expr
expr = tokens . takeExpr . endOfTokens

p :: Proxy String
p = Proxy

takeExpr :: VerbosePrism' String [String] (Expr, [String])
takeExpr =
    infixOpLeftRecursion p "+" _Add $           -- Additions of
    infixOpLeftRecursion p "*" _Mul $           -- multiplications of
    tryMatchAtom p (prismFallback _Lit) _Show $ -- literals or
    parens takeExpr                             -- expressions in parens

printNice :: Show a => Either String a -> IO ()
printNice = putStrLn . either id show

main :: IO ()
main =
    do
        putStrLn ("1 + ((2 + 3 + 9)*4) + 5" & expr %~ id)
        printNice ("1 + ((2 + 3 + 9)*4) + 5" ^?? expr)
        printNice ("(1 + (2*3)" ^?? expr)
        printNice ("1 + (2*3" ^?? expr)
        printNice ("1 + (2*3) 3 + (4 * 5)" ^?? expr)
        printNice (") 1 + (2*3)" ^?? expr)
        printNice ("a + (2*3)" ^?? expr)
