module SyntaxOptics
    ( tokens
    , infixOpLeftRecursion, tryMatch
    , asideFirst, firstOnly, secondOnly
    ) where

import Control.Lens
import Data.Char as Char
import SyntaxOptics.LensExtras

-- Extend a base parsing prism with applications of an operator
infixOpLeftRecursion ::
    (Choice p, Applicative f, Eq a) =>
    a ->                         -- The operator's text
    APrism' expr (expr, expr) -> -- The operator constructor's prism
    APrism' [a] (expr, [a]) ->   -- The base parsing prism
    Optic' p f [a] (expr, [a])
infixOpLeftRecursion operatorText c sub =
    leftRecursion c
    (aside (_Cons . firstOnly operatorText . sub) . retuple)
    sub

-- Extend a base parsing prism with extensions to its right side
leftRecursion ::
    (Choice p, Applicative f) =>
    APrism' whole cons ->
    APrism' (whole, state) (cons, state) ->
    APrism' state (whole, state) ->
    Optic' p f state (whole, state)
leftRecursion c extend base =
    prism' build (fmap parseExtends . (^? clonePrism base))
    where
        build (x, state) =
            maybe
            (clonePrism base # (x, state))
            (build . (clonePrism extend #) . (, state)) (x ^? clonePrism c)
        parseExtends x =
            x ^? clonePrism extend <&> _1 %~ (clonePrism c #) & maybe x parseExtends

-- Transform a string into tokens
tokens :: Iso' String [String]
tokens =
    iso splitTokens (foldr addToken "")
    where
        addToken x "" = x
        addToken [x] y
            | Char.generalCategory x == Char.OpenPunctuation = x : y
        addToken x (y:ys)
            | Char.generalCategory y == Char.ClosePunctuation = x <> (y:ys)
        addToken x y = x <> " " <> y
        isOp =
            (`elem` [Char.MathSymbol, Char.OtherPunctuation]) .
            Char.generalCategory
        isParen = (`elem` "()[]{}")
        splitTokens "" = []
        splitTokens (x:s:xs) | Char.isSpace s = [x] : splitTokens xs
        splitTokens (s:xs) | Char.isSpace s = splitTokens xs
        splitTokens (x:xs) | isParen x = [x] : splitTokens xs
        splitTokens (x:xs) =
            case splitTokens xs of
            [] -> [[x]]
            ((y:ys) : zs) | not (isParen y) && isOp x == isOp y -> (x:y:ys) : zs
            ys -> [x] : ys
