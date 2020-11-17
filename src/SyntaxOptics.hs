{-# LANGUAGE RankNTypes #-}

module SyntaxOptics
    ( tokens
    , tryMatch
    , infixOpLeftRecursion
    , asideFirst, firstOnly, secondOnly
    ) where

import Control.Applicative (Alternative(..))
import Control.Lens
import Data.Char as Char

firstOnly :: Eq e => e -> Prism' (e, a) a
firstOnly x = asideFirst (only x) . iso snd (() ,)

secondOnly :: Eq e => e -> Prism' (a, e) a
secondOnly x = swapped . firstOnly x

asideFirst :: APrism s t a b -> Prism (s, e) (t, e) (a, e) (b, e)
asideFirst l = swapped . aside l . swapped

-- Tuple shuffling Iso
retuple ::
    Iso
    (a0, (a1, a2)) (b0, (b1, b2))
    ((a0, a1), a2) ((b0, b1), b2)
retuple =
    iso
    (\(w0, (w1, r)) -> ((w0, w1), r))
    (\((w0, w1), r) -> (w0, (w1, r)))

-- Extend a base parsing prism with applications of an operator
infixOpLeftRecursion ::
    Eq a =>
    a ->                        -- The operator's text
    Prism' expr (expr, expr) -> -- The operator constructor's prism
    Prism' [a] (expr, [a]) ->   -- The base parsing prism
    Prism' [a] (expr, [a])
infixOpLeftRecursion operatorText c sub =
    leftRecursion c
    (aside (_Cons . firstOnly operatorText . sub) . retuple)
    sub

-- Extend a base parsing prism with extensions to its right side
leftRecursion ::
    Prism' whole cons ->
    Prism' (whole, state) (cons, state) ->
    Prism' state (whole, state) ->
    Prism' state (whole, state)
leftRecursion c extend base =
    prism' build (fmap parseExtends . (^? base))
    where
        build (x, state) =
            maybe
            (base # (x, state))
            (build . (extend #) . (, state)) (x ^? c)
        parseExtends x =
            x ^? extend <&> _1 %~ (c #) & maybe x parseExtends

-- Add an encoding for a sum-type constructor to an existing prism
tryMatch ::
    Prism' whole cons -> -- The sum-type constructor prism
    Prism' src cons ->   -- Parse the constructor contents
    Prism' src whole ->  -- Prism to encode the other options
    Prism' src whole
tryMatch c parse fallback =
    prism' build (\x -> (x ^? parse <&> (c #)) <|> x ^? fallback)
    where
        build x = maybe (fallback # x) (parse #) (x ^? c)

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
