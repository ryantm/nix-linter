module Nix.Linter.Utils where

import           Control.Arrow      ((***))
import           Control.Monad.Free
import           Data.Fix
import           Data.Foldable      (toList)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap


choose :: [a] -> [(a, [a])]
choose []       = []
choose (x : xs) = (x, xs) : ((x :) <$$> choose xs)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

fixToFree :: Functor f => Fix f -> Free f a
fixToFree (Fix e) = Free (fixToFree <$> e)

freeToFix :: Functor f => (a -> Free f a) -> Free f a -> Fix f
freeToFix f (Free x) = Fix (freeToFix f <$> x)
freeToFix f (Pure x) = freeToFix f $ f x

freeToFix' :: Functor f => (a -> Fix f) -> Free f a -> Fix f
freeToFix' = undefined

subTrees :: Foldable f => Fix f -> [Fix f]
subTrees e = e : (subTrees =<< (toList $ unFix e))

fUnzip :: (Functor f) => f (a, b) -> (f a, f b)
fUnzip xs = (fmap fst xs, fmap snd xs)

chooseFree :: Traversable f => Fix f -> [(Fix f, Free f ())]
chooseFree (Fix e) = (Fix e, Pure ()) : ((Fix *** Free) . fUnzip <$> (traverse chooseFree e))
