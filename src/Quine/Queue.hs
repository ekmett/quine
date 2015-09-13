{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Quine.Queue
  ( Queue(..)
  , fromList
  , null
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Data
#if MIN_VERSION_base(4,8,0)
import Data.Foldable hiding (null)
#else
import Data.Foldable
#endif
import Data.Function (on)
import Data.Traversable
import GHC.Generics hiding (prec)
import Prelude hiding (null)
import Text.Read

-- an _ephemeral_ work queue
data Queue a = Queue [a] [a]
  deriving (Functor, Data, Typeable, Generic) 

instance Show a => Show (Queue a) where
  showsPrec d q = showParen (d > 10) $
    showString "fromList " . showsPrec 11 (toList q)

instance Read a => Read (Queue a) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    m <- step readPrec
    return (fromList m)

instance Eq a => Eq (Queue a) where
  (==) = (==) `on` toList

instance Ord a => Ord (Queue a) where
  compare = compare `on` toList

fromList :: [a] -> Queue a 
fromList fs = Queue fs []

null :: Queue a -> Bool
null (Queue [] []) = True
null _ = False

instance Applicative Queue where
  pure a = Queue [a] []
  fs <*> as = fromList [ f a | f <- toList fs, a <- toList as ]

instance Alternative Queue where
  as <|> Queue fs rs = Queue (toList as ++ fs) rs
  empty = Queue [] [] -- the whole point of these instances!

instance Monad Queue where
  return a = Queue [a] []
  m >>= f = fromList [ b | a <- toList m, b <- toList (f a) ]

instance MonadPlus Queue where
  mplus = (<|>)
  mzero = empty

instance Foldable Queue where
  foldMap = foldMapDefault

instance Traversable Queue where
  traverse f (Queue fs rs) = Queue <$> traverse f fs <*> backwards traverse f rs

instance Cons (Queue a) (Queue b) a b where
  _Cons = prism (\(a,Queue as bs) -> Queue (a:as) bs) $ \case
    Queue (a:as) bs -> Right (a, Queue as bs)
    Queue [] bs -> case reverse bs of
      [] -> Left (Queue [] [])
      (a:as) -> Right (a, Queue as [])

instance Snoc (Queue a) (Queue b) a b where
  _Snoc = prism (\(Queue as bs,b) -> Queue as (b:bs)) $ \case
    Queue as (b:bs) -> Right (Queue as bs, b)
    Queue as [] -> case reverse as of
      []     -> Left (Queue [] [])
      (b:bs) -> Right (Queue [] bs, b)
