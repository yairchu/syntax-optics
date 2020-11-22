{-# LANGUAGE RankNTypes #-}

module SyntaxOptics
    ( tokens, endOfTokens
    , infixOpLeftRecursion
    , parens, tryMatchAtom
    , prismFallback
    , many
    ) where

import Control.Lens
import Control.Monad ((>=>))
import Data.Char as Char
import SyntaxOptics.LensExtras
import VerboseOptics
import Data.Proxy (Proxy(..))

-- Extend a base parsing prism with applications of an operator
infixOpLeftRecursion ::
    (Choice p, VerboseApplicative e f, Eq a) =>
    Proxy e ->
    a ->                               -- The operator's text
    APrism' expr (expr, expr) ->       -- The operator constructor's prism
    VerbosePrism' e [a] (expr, [a]) -> -- The base parsing prism
    Optic' p f [a] (expr, [a])
infixOpLeftRecursion _ operatorText c sub =
    verbosePrism build (matchingVerbose sub >=> parseExtends)
    where
        build (x, state) =
            maybe
            (sub # (x, state))
            (\(left, right) -> build (left, [operatorText]) <> build (right, state))
            (x ^? clonePrism c)
        parseExtends (base, s0) =
            case s0 ^? _Cons . asideFirst (only operatorText) of
            Nothing -> Right (base, s0)
            Just ((), s1) ->
                matchingVerbose sub s1 & _Right . _1 %~ (clonePrism c #) . (base, )
                >>= parseExtends

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
endOfTokens = secondOnly (\x -> "Unexpected at end: " <> show (tokens # x)) mempty

takeItem ::
    Cons s t a b =>
    e -> VerbosePrism e s t (a, s) (b, t)
takeItem e = verbose (const e) _Cons

expect ::
    (Cons t t b b, Choice p, VerboseApplicative String f, Eq b, Show b) =>
    b -> p t (f t) -> p t (f t)
expect x =
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
    VerbosePrism' String [String] (x, [String]) ->
    VerbosePrism' String [String] (x, [String])
parens p = expect "(" . p . verboseAside (Proxy @String) (expect ")")

tupleInEither ::
    Iso
    (Either a0 a1, x) (Either b0 b1, x)
    (Either (a0, x) (a1, x)) (Either (b0, x) (b1, x))
tupleInEither =
    iso
    (\(a, x) -> bimap (, x) (, x) a)
    (either (_1 %~ Left) (_1 %~ Right))

tryMatchAtom ::
    Cons t t r0 r1 =>
    Proxy e ->
    AnIso b a (Either o1 c1) (Either o0 c0) ->
    APrism r0 r1 c0 c1 ->
    VerbosePrism e t t (o0, t) (o1, t) ->
    VerbosePrism e t t (a, t) (b, t)
tryMatchAtom p con repr =
    tryMatch p (bimapping con id . tupleInEither) (_Cons . asideFirst repr)

many :: APrism' [a] (r, [a]) -> Iso' [a] ([r], [a])
many single =
    iso parse build
    where
        build ([], rest) = rest
        build (x:xs, rest) = clonePrism single # (x, build (xs, rest))
        parse src =
            case src ^? clonePrism single of
            Nothing -> ([], src)
            Just (x, rest) -> parse rest & _1 %~ (x:)
