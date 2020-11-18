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

takeExpr :: VerbosePrism' String [String] (Expr, [String])
takeExpr =
    infixOpLeftRecursion p "+" _Add $ -- Additions of
    infixOpLeftRecursion p "*" _Mul $ -- multiplications of
    tryMatchAtom p _Lit _Show $       -- literals or
    parens takeExpr                   -- expressions in parens
    where
        p = Proxy @String

main :: IO ()
main =
    do
        putStrLn ("1 + ((2 + 3)*4)" & expr %~ id)
        printNice ("1 + (2*3)" ^?? expr)
        printNice ("(1 + (2*3)" ^?? expr)
        printNice ("1 + (2*3" ^?? expr)
        printNice ("1 + (2*3) 3" ^?? expr)
        printNice (") 1 + (2*3)" ^?? expr)
        printNice ("a + (2*3)" ^?? expr)
    where
        printNice = putStrLn . either id show
