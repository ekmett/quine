{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Quine.Pointwise
  ( smoothstep, smoothstep1
  , clamp, clamp1
  , mix, mix1
  , step, step1
  , lessThan, lessThanEqual
  , greaterThan, greaterThanEqual
  , equal, notEqual
  ) where

import Control.Applicative

clamp :: (Applicative f, Ord a) => f a -> f a -> f a -> f a
clamp = liftA3 clamp1 where 

clamp1 :: Ord a => a -> a -> a -> a
clamp1 x l h = min (max x l) h

smoothstep :: (Applicative f, Fractional a, Ord a) => f a -> f a -> f a -> f a
smoothstep = liftA3 smoothstep1

smoothstep1 :: (Fractional a, Ord a) => a -> a -> a -> a
smoothstep1 l h x = t*t*(3-2*t) where
  t = clamp1 ((x-l)/(h-l)) 0 1

mix :: (Applicative f, Num a) => f a -> f a -> f a -> f a
mix = liftA3 mix1

mix1 :: Num a => a -> a -> a -> a
mix1 x y a = x*(1-a)+y*a

step :: (Applicative f, Num a, Ord a) => f a -> f a -> f a
step = liftA2 step1

step1 :: (Num a, Ord a) => a -> a -> a 
step1 edge x 
  | x < edge  = 0
  | otherwise = 1

lessThan :: (Applicative f, Ord a) => f a -> f a -> f Bool
lessThan = liftA2 (<)

lessThanEqual :: (Applicative f, Ord a) => f a -> f a -> f Bool
lessThanEqual = liftA2 (<=)

greaterThan :: (Applicative f, Ord a) => f a -> f a -> f Bool
greaterThan = liftA2 (>)

greaterThanEqual :: (Applicative f, Ord a) => f a -> f a -> f Bool
greaterThanEqual = liftA2 (>=)

equal ::  (Applicative f, Eq a) => f a -> f a -> f Bool
equal = liftA2 (==)

notEqual ::  (Applicative f, Eq a) => f a -> f a -> f Bool
notEqual = liftA2 (/=)
