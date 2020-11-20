module SyntaxOptics.LensExtras
    ( reviewing
    , asideFirst
    , retuple
    , prismFallback
    ) where

import Control.Lens
import Data.Tagged (Tagged)

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
-- TODO:
-- This was added to lens in 2020.11.20
-- https://github.com/ekmett/lens/commit/d47a6e9213f218a3d8f47eca557af018b9fa9aea
-- Remove from here once available from next release in hackage
reviewing :: (Bifunctor p, Functor f) => Optic Tagged Identity s t a b -> Optic' p f t b
reviewing p =
    bimap f (fmap f)
    where
        f = _Unwrapped . _Unwrapped %~ p

prismFallback ::
    (Profunctor p, Functor f) =>
    APrism s t a b ->
    Optic p f s t (Either s a) (Either t b)
prismFallback p =
    iso parse (either id (reviewing (clonePrism p) #))
    where
        parse x = maybe (Left x) Right (x ^? getting (clonePrism p))
