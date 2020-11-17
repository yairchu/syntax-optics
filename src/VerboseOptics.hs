{-# LANGUAGE RankNTypes, TemplateHaskell, TypeFamilies #-}

module VerboseOptics
    ( (^??), matchingVerbose
    , ParseResult(..), _ParseSuccess, _ParseFail
    , VerboseApplicative(..)
    , VerboseTraversal, VerbosePrism, VerbosePrism'
    , verbose, Lift(..)
    , verbosePrism
    ) where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply

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

instance Apply f => Applicative (Lift f) where
    pure = Pure
    Pure f <*> Pure x = Pure (f x)
    Pure f <*> Other x = Other (f <$> x)
    Other f <*> Pure x = Other (f ?? x)
    Other f <*> Other x = Other (f <.> x)

verbosePrism :: (b -> t) -> (s -> Either (e, t) a) -> VerbosePrism e s t a b
verbosePrism bt seta = dimap seta (either (uncurry vpure) (fmap bt)) . right'
