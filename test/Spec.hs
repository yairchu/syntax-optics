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
    -- Additions of
    infixOpLeftRecursion p "+" _Add $
    -- multiplications of
    infixOpLeftRecursion p "*" _Mul $
    verbose (\x -> "Unexpected: " <> unwords (take 1 x)) $
    -- literals or
    tryMatch (asideFirst _Lit) (_Cons . asideFirst _Show) $
    -- expressions in parentheses
    expect "(" . takeExpr . aside (expect ")")
    where
        p :: Proxy String
        p = Proxy

main :: IO ()
main =
    do
        putStrLn ("1 + (2*3)" & expr %~ id)
        print ("1 + (2*3) 3" ^?? expr)
        print ("1 + (2*3" ^?? expr)
        print (") 1 + (2*3)" ^?? expr)
