{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.Char
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
    infixOpLeftRecursion p "+" _Add $           -- Additions of
    infixOpLeftRecursion p "*" _Mul $           -- multiplications of
    tryMatchAtom p (prismFallback _Lit) _Show $ -- literals or
    parens takeExpr                             -- expressions in parens
    where
        p = Proxy @String

data Lisp
    = LAtom String
    | LList [Lisp]
    deriving (Show, Eq)
makePrisms ''Lisp

atomOrList :: Iso' Lisp (Either [Lisp] String)
atomOrList =
    iso toEither (either LList LAtom)
    where
        toEither (LAtom x) = Right x
        toEither (LList x) = Left x

lisp :: VerbosePrism' String String Lisp
lisp = tokens . takeLisp . endOfTokens

takeLisp :: VerbosePrism' String [String] (Lisp, [String])
takeLisp =
    tryMatchAtom (Proxy @String) atomOrList (filtered (all isAlphaNum)) $
    parens (many takeLisp)

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

        printNice ("(1 2 (3 4) 5)" ^?? lisp)
        printNice ("(1 2 (3 4)) 5)" ^?? lisp)
        printNice ("(1 // (3 4) 5)" ^?? lisp)
