{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Engine.Var 
  ( Invariant(xmap)
  , Varied(vary)
  , Setting(Setting)
  , (&=), (&~), (&~!), (&=!)
  ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.Functor
import Data.Functor.Contravariant
import Data.Monoid
import qualified Graphics.Rendering.OpenGL.GL.StateVar as GL

class Invariant f where
  xmap :: (b -> a) -> (a -> b) -> f a -> f b
  default xmap :: Functor f => (b -> a) -> (a -> b) -> f a -> f b
  xmap _ = fmap

instance Invariant IO
instance Monad m => Invariant (ReaderT e m) where xmap _ = liftM
instance Monad m => Invariant (StateT s m) where xmap _ = liftM
instance (Monad m, Monoid w) => Invariant (WriterT w m) where xmap _ = liftM

class Invariant f => Varied f where
  vary :: IO a -> (a -> IO ()) -> f a
  default vary :: MonadIO f => IO a -> (a -> IO ()) -> f a
  vary = const . liftIO
  
instance Varied IO
instance MonadIO m => Varied (ReaderT e m) 
instance MonadIO m => Varied (StateT s m)
instance (MonadIO m, Monoid w) => Varied (WriterT w m) 

newtype Setting a = Setting (a -> IO ())

instance Contravariant Setting where
  contramap f (Setting g) = Setting (g . f)

instance Invariant Setting where
  xmap f _ = contramap f

instance Varied Setting where
  vary _ = Setting

-- legacy support

instance Invariant GL.StateVar where
  xmap f g v = GL.makeStateVar (g <$> GL.get v) ((GL.$=) v . f)

instance Varied GL.StateVar where
  vary = GL.makeStateVar

instance Invariant GL.SettableStateVar where
  xmap f _ = contramap f

instance Varied GL.SettableStateVar where
  vary _ = GL.makeSettableStateVar

instance Invariant GL.GettableStateVar

instance Varied GL.GettableStateVar where
  vary = const . GL.makeGettableStateVar

-- orphan instances

instance Contravariant GL.SettableStateVar where
  contramap f v = GL.makeSettableStateVar ((GL.$=) v . f)

-- this should move into OpenGL
instance Functor GL.GettableStateVar where
  fmap f v = GL.makeGettableStateVar (f <$> GL.get v)

(&~) :: MonadIO m => GL.StateVar a -> (a -> a) -> m ()
v &~ f = liftIO $ do
   x <- GL.get v
   v GL.$= f x

(&=) :: MonadIO m => Setting a -> a -> m ()
Setting v &= x = liftIO $ v x

-- | A variant of '&=' which is strict in the value to be set.
(&=!) :: MonadIO m => Setting a -> a -> m ()
v &=! x = liftIO $ x `seq` v &= x

-- | A variant of '&~' which is strict in the transformed value.
(&~!) :: MonadIO m => GL.StateVar a -> (a -> a) -> m ()
v &~! f = liftIO $ do
   x <- GL.get v
   v GL.$=! f x
