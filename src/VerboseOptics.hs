{-# LANGUAGE RankNTypes, TemplateHaskell, TypeFamilies #-}

module VerboseOptics
    ( (^??), matchingVerbose
    , ParseResult(..), _ParseSuccess, _ParseFail
    , VerboseApplicative(..)
    , VerboseTraversal, VerbosePrism, VerbosePrism'
    , verbose, Lift(..)
    , verbosePrism
    , verboseWithout, verboseAside, verboseAsideFirst
    , tryMatch
    ) where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply (Apply((<.>)))
import Data.Proxy (Proxy(..))
import SyntaxOptics.LensExtras (reviewing)

data ParseResult e r a
    = ParseSuccess r
    | ParseFail e a
    deriving Functor
makePrisms ''ParseResult

infixl 8 ^??
(^??) :: s -> LensLike' (ParseResult e a) s a -> Either e a
whole ^?? f =
    matchingVerbose f whole & _Left %~ fst

-- | Retrieve the value targeted by a 'VerbosePrism' or return the
-- original value while allowing the type to change if it does
-- not match.
matchingVerbose :: LensLike (ParseResult e a) s t a b -> s -> Either (e, t) a
matchingVerbose f =
    \case
    ParseSuccess b -> Right b
    ParseFail e t -> Left (e, t)
    . f ParseSuccess

class Apply f => VerboseApplicative e f where
    vpure :: e -> a -> f a

type VerboseTraversal e s t a b =
    forall f.
    VerboseApplicative e f =>
    LensLike f s t a b

-- TODO: Also create the AVerbosePrism type
type VerbosePrism e s t a b =
    forall p f.
    (Choice p, VerboseApplicative e f) =>
    Optic p f s t a b

type VerbosePrism' e s a = VerbosePrism e s s a a

-- Verbose optics support for (^.) and (^..)
instance Monoid r => VerboseApplicative e (Const r) where
    vpure _ = pure

-- Verbose optics support for `preview`, aka (#)
instance VerboseApplicative e Identity where
    vpure _ = pure

-- Verbose optics support for our (^??)
instance Apply (ParseResult e r) where
    ParseSuccess x <.> _ = ParseSuccess x
    _ <.> ParseSuccess x = ParseSuccess x
    ParseFail e f <.> ParseFail _ x = ParseFail e (f x)

instance e ~ e' => VerboseApplicative e (ParseResult e' r) where
    vpure = ParseFail

-- Given an error message constructor, turns:
-- * Traversal to VerboseTraversal
-- * Prism to VerbosePrism
verbose ::
    (Profunctor p, VerboseApplicative e f) =>
    (t -> e) ->
    Optic p (Lift f) s t a b ->
    Optic p f s t a b
verbose e t =
    rmap f . t . rmap Other
    where
        f (Other r) = r
        f (Pure r) = vpure (e r) r

-- A fixed variant of transformers:Control.Applicative.Lift -
-- Turns an Apply to an Applicative
-- (transformer's versions Applicative instance requires Applicative f)
data Lift f a = Pure a | Other (f a)
    deriving Functor

instance Apply f => Apply (Lift f) where
    Pure f <.> Pure x = Pure (f x)
    Pure f <.> Other x = Other (f <$> x)
    Other f <.> Pure x = Other (f ?? x)
    Other f <.> Other x = Other (f <.> x)

instance Apply f => Applicative (Lift f) where
    pure = Pure
    (<*>) = (<.>)

-- Used when changing the error message for an already verbose prism
instance VerboseApplicative e f => VerboseApplicative e (Lift f) where
    vpure e = Other . vpure e -- TODO: is this the right?

verbosePrism :: (b -> t) -> (s -> Either (e, t) a) -> VerbosePrism e s t a b
verbosePrism bt seta = dimap seta (either (uncurry vpure) (fmap bt)) . right'

-- | Given a pair of verbose prisms, project sums.
--
-- TODO: Implement with AVerbosePrism type so won't need Proxy
verboseWithout ::
    Proxy e ->
    VerbosePrism e s0 t0 a0 b0 ->
    VerbosePrism e s1 t1 a1 b1 ->
    VerbosePrism e (Either s0 s1) (Either t0 t1) (Either a0 a1) (Either b0 b1)
verboseWithout _ p0 p1 =
    verbosePrism (either (Left . (reviewing p0 #)) (Right . (reviewing p1 #))) $
    \case
    Left x -> matchingVerbose p0 x & _Right %~ Left & _Left . _2 %~ Left
    Right x -> matchingVerbose p1 x & _Right %~ Right & _Left . _2 %~ Right

verboseAside ::
    Proxy e ->
    VerbosePrism e s t a b ->
    VerbosePrism e (x, s) (x, t) (x, a) (x, b)
verboseAside _ p =
    verbosePrism (_2 %~ (reviewing p #)) $
    \(x, s) -> matchingVerbose p s & _Right %~ (x, ) & _Left . _2 %~ (x, )

verboseAsideFirst ::
    Proxy e ->
    VerbosePrism e s t a b ->
    VerbosePrism e (s, x) (t, x) (a, x) (b, x)
verboseAsideFirst p l = swapped . verboseAside p l . swapped

-- Add an encoding for a sum-type constructor to an existing verbose prism
tryMatch ::
    Proxy e ->
    APrism b a c1 c0 -> -- The sum-type constructor prism
    APrism s t c0 c1 -> -- Parse the constructor contents
    VerbosePrism e s t a b ->   -- Prism to encode the other options
    VerbosePrism e s t a b
tryMatch _ c p fallback =
    verbosePrism build match
    where
        build x =
            maybe
            (reviewing fallback # x)
            (reviewing (clonePrism p) #)
            (x ^? getting (clonePrism c))
        match x =
            case x ^? getting (clonePrism p) of
            Just y -> Right (reviewing (clonePrism c) # y)
            Nothing -> matchingVerbose fallback x
