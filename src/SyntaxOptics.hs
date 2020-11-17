{-# LANGUAGE RankNTypes #-}
module SyntaxOptics
    ( tokens, endOfTokens, expect
    , infixOpLeftRecursion, tryMatch
    , asideFirst, firstOnly, secondOnly
    ) where

import Control.Lens
import Data.Char as Char
import SyntaxOptics.LensExtras
import VerboseOptics
import Data.Proxy

-- Extend a base parsing prism with applications of an operator
infixOpLeftRecursion ::
    (Choice p, VerboseApplicative e f, Eq a) =>
    Proxy e ->
    a ->                               -- The operator's text
    APrism' expr (expr, expr) ->       -- The operator constructor's prism
    VerbosePrism' e [a] (expr, [a]) -> -- The base parsing prism
    Optic' p f [a] (expr, [a])
infixOpLeftRecursion p operatorText c sub =
    leftRecursion p c
    (aside (_Cons . firstOnly operatorText . sub) . retuple)
    sub

-- Extend a base parsing prism with extensions to its right side
leftRecursion ::
    Proxy e ->
    APrism' whole cons ->
    APrism' (whole, state) (cons, state) ->
    VerbosePrism' e state (whole, state) ->
    VerbosePrism' e state (whole, state)
leftRecursion _ c extend base =
    verbosePrism build (fmap parseExtends . matchingVerbose base)
    where
        build (x, state) =
            maybe
            (base # (x, state))
            (build . (clonePrism extend #) . (, state))
            (x ^? clonePrism c)
        parseExtends x =
            x ^? clonePrism extend
            <&> _1 %~ (clonePrism c #) & maybe x parseExtends

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

endOfTokens :: VerbosePrism' String (a, [String]) a
endOfTokens = verbose (\(_, x) -> "Expected end of file, got " <> unwords x) (secondOnly mempty)

expect :: (Cons t t b b, Choice p, Applicative f, Eq b) => b -> p t (f t) -> p t (f t)
expect x = _Cons . firstOnly x
