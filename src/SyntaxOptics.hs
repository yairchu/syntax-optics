{-# LANGUAGE RankNTypes #-}
module SyntaxOptics
    ( tokens, endOfTokens
    , infixOpLeftRecursion
    , parens, tryMatchAtom
    ) where

import Control.Lens
import Data.Char as Char
import SyntaxOptics.LensExtras
import VerboseOptics
import Data.Proxy

-- Extend a base parsing prism with applications of an operator
infixOpLeftRecursion ::
    (Choice p, VerboseApplicative e f, Eq a, Show a) =>
    Proxy e ->
    a ->                               -- The operator's text
    APrism' expr (expr, expr) ->       -- The operator constructor's prism
    VerbosePrism' e [a] (expr, [a]) -> -- The base parsing prism
    Optic' p f [a] (expr, [a])
infixOpLeftRecursion p operatorText c sub =
    leftRecursion p c
    (aside (expect operatorText . sub) . retuple)
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
endOfTokens = secondOnly (\x -> "Unexpected at end: " <> show (unwords x)) mempty

takeItem ::
    Cons s t a b =>
    e -> VerbosePrism e s t (a, s) (b, t)
takeItem e = verbose (const e) _Cons

expect ::
    (Cons t t b b, Choice p, VerboseApplicative String f, Eq b, Show b) =>
    b -> p t (f t) -> p t (f t)
expect x =
    takeItem ("Ended when expected " <> show x) .
    firstOnly (const ("Expected " <> show x)) x

expect' ::
    (Cons t t b b, Choice p, VerboseApplicative String f, Eq b, Show b) =>
    b -> p t (f t) -> p t (f t)
expect' x =
    takeItem ("Ended when expected " <> show x) .
    firstOnly (\y -> "Unexpected " <> show y) x

verboseOnly ::
    (Eq a, Choice p, VerboseApplicative e f) =>
    (a -> e) -> a -> Optic' p f a ()
verboseOnly e a =
    verbosePrism (const a) parse
    where
        parse x
            | x == a = Right ()
            | otherwise = Left (e x, x)

firstOnly ::
    forall p e f a b.
    (Choice p, VerboseApplicative e f, Eq a) =>
    (a -> e) -> a -> Optic' p f (a, b) b
firstOnly e x = verboseAsideFirst (Proxy @e) (verboseOnly e x) . iso snd (() ,)

secondOnly ::
    (Choice p, VerboseApplicative e f, Eq a) =>
    (a -> e) -> a -> Optic' p f (b, a) b
secondOnly e x = swapped . firstOnly e x

parens ::
    VerbosePrism' String [String] (x, [String]) -> VerbosePrism' String [String] (x, [String])
parens p = expect' "(" . p . verboseAside (Proxy @String) (expect ")")

tryMatchAtom ::
    Cons t t r0 r1 =>
    Proxy e ->
    APrism b a c1 c0 ->
    APrism r0 r1 c0 c1 ->
    VerbosePrism e t t (a, t) (b, t) ->
    VerbosePrism e t t (a, t) (b, t)
tryMatchAtom p con repr = tryMatch p (asideFirst con) (_Cons . asideFirst repr)
