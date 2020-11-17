module SyntaxOptics.LensExtras
    ( firstOnly, secondOnly
    , asideFirst
    , retuple
    , tryMatch
    ) where

import Control.Applicative (Alternative(..))
import Control.Lens

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

-- Add an encoding for a sum-type constructor to an existing prism
tryMatch ::
    (Choice p, Applicative f) =>
    APrism' whole cons -> -- The sum-type constructor prism
    APrism' src cons ->   -- Parse the constructor contents
    APrism' src whole ->  -- Prism to encode the other options
    Optic' p f src whole
tryMatch c p fallback =
    prism' build parse
    where
        build x = maybe (clonePrism fallback # x) (clonePrism p #) (x ^? clonePrism c)
        parse x = (x ^? clonePrism p <&> (clonePrism c #)) <|> x ^? clonePrism fallback
