{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import SyntaxOptics

data Expr
    = Lit Int
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Show, Eq)
makePrisms ''Expr

expr :: Prism' String Expr
expr =
    tokens .      -- convert string to tokens
    takeExpr .    -- take the expression
    secondOnly [] -- and there should be no remaining tokens

takeExpr :: Prism' [String] (Expr, [String])
takeExpr =
    infixOpLeftRecursion "+" _Add $ -- Additions of
    infixOpLeftRecursion "*" _Mul $ -- multiplications of
    tryMatch (asideFirst _Lit)      -- literals or
        (_Cons . asideFirst _Show) $
    _Cons . firstOnly "(" .         -- expressions in parentheses
        takeExpr . aside (_Cons . firstOnly ")")

main :: IO ()
main = putStrLn ("1 + (2*3)" & expr %~ id)
