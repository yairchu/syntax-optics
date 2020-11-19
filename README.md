# Syntax Optics: Parsing and Pretty-Printing with Prisms

Optics for parsing and pretty-pretting programming language syntax.

These are combinators that allow describing syntax declaratively:

```Haskell
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
```

`expr` defined above can be used to

```Haskell
> -- Use "#" from "lens" to pretty-print an Expr into a String
> expr # (Lit 1 `Mul` (Lit 2 `Add` Lit 3)) `Add` Lit 4
"1 * (2 + 3) + 4"

> -- Use "^?" from "lens" to try to parse an expression from a String to a Maybe Expr
> "1 * (2 + 3) + 4" ^? expr
Just (Add (Mul (Lit 1) (Add (Lit 2) (Lit 3))) (Lit 4))
> "1 (2 + 3) + 4" ^? expr
Nothing

> -- Use the new "^??" to get readable parse errors when the parsing fails
> "2 + 3" ^?? expr
Right (Add (Lit 2) (Lit 3))
> "2 + 3 3" ^?? expr
Left "Unexpected at end: \"3\""
```

Note that `VerbosePrism`s are compatible and composable with other optics!
