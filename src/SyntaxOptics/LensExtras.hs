module SyntaxOptics.LensExtras
    ( firstOnly, secondOnly
    , asideFirst
    , retuple
    , tryMatch
    ) where

import Control.Lens
import Data.Tagged (Tagged)

firstOnly ::
    (Choice p, Applicative f, Eq e) =>
    e -> Optic' p f (e, a) a
firstOnly x = asideFirst (only x) . iso snd (() ,)

secondOnly ::
    (Choice p, Applicative f, Eq e) =>
    e -> Optic' p f (a, e) a
secondOnly x = swapped . firstOnly x

asideFirst ::
    (Choice p, Applicative f) =>
    APrism s t a b -> Optic p f (s, e) (t, e) (a, e) (b, e)
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

-- | Coerce a polymorphic 'Prism' to a 'Review'.
--
-- @
-- 'reviewing' :: 'Iso' s t a b -> 'Review' t b
-- 'reviewing' :: 'Prism' s t a b -> 'Review' t b
-- @
--
-- From https://github.com/ekmett/lens/pull/906
reviewing :: (Bifunctor p, Functor f) => Optic Tagged Identity s t a b -> Optic' p f t b
reviewing p =
    bimap f (fmap f)
    where
        f = _Unwrapped . _Unwrapped %~ p

-- Add an encoding for a sum-type constructor to an existing prism
tryMatch ::
    (Choice p, Applicative f) =>
    APrism b a c1 c0 -> -- The sum-type constructor prism
    APrism s t c0 c1 -> -- Parse the constructor contents
    APrism s t a b ->   -- Prism to encode the other options
    Optic p f s t a b
tryMatch c p fallback =
    prism build parse
    where
        build x =
            maybe
            (reviewing (clonePrism fallback) # x)
            (reviewing (clonePrism p) #)
            (x ^? getting (clonePrism c))
        parse x =
            case x ^? getting (clonePrism p) of
            Just y -> Right (reviewing (clonePrism c) # y)
            Nothing -> matching (clonePrism fallback) x
