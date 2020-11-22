module SyntaxOptics.LensExtras
    ( reviewing
    , asideFirst
    , prismFallback
    ) where

import Control.Lens
import Data.Tagged (Tagged)

asideFirst ::
    (Choice p, Applicative f) =>
    APrism s t a b -> Optic p f (s, e) (t, e) (a, e) (b, e)
asideFirst l = swapped . aside l . swapped

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

-- Available in various packages such as "either"
maybeToRight :: b -> Maybe a -> Either b a
maybeToRight x Nothing = Left x
maybeToRight _ (Just x) = Right x

prismFallback ::
    (Profunctor p, Functor f) =>
    APrism s t a b ->
    Optic p f s t (Either s a) (Either t b)
prismFallback p =
    iso (maybeToRight <*> (^? getting (clonePrism p))) (either id (reviewing (clonePrism p) #))
