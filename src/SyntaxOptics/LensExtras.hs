module SyntaxOptics.LensExtras
    ( asideFirst
    , prismFallback
    ) where

import Control.Lens

asideFirst ::
    (Choice p, Applicative f) =>
    APrism s t a b -> Optic p f (s, e) (t, e) (a, e) (b, e)
asideFirst l = swapped . aside l . swapped

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
