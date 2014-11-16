{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- The GPU is an asynchronous resource, but we only have one controlling thread..
-- 
-- We can poll the GPU at various sync points
module Quine.Task
  ( MonadTask(..)
  , barrier
  , Fence(..)
  , Task(..)
  , execTask
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Word
import Foreign.Ptr
import Graphics.GL.Types
import Graphics.GL
import Quine.GL.Error

data Fence = Fence !Int !GLsync

instance Eq Fence where
  Fence a _ == Fence b _ = a == b

instance Ord Fence where
  Fence a _ `compare` Fence b _ = compare a b

class MonadIO m => MonadTask m where
  -- | Start a task, this task may be run out of order with your own context. 
  --
  -- After running this command, assume that the context is scrambled.
  --
  -- After running the returned command, also assume that the context is scrambled.
  spawn :: m a -> m (m a)

  -- | Create a synchronization point, later we can use the action we get to block
  --
  -- This does not block and does not scramble the context.
  fence :: m Fence

  -- | Assume the context is scrambled after this returns
  await :: Fence -> m ()

-- | Wait until all commands up to a given point have been completed
--
-- Meanwhile other tasks may run.
--
-- When you get control back, assume that the context is scrambled
--
-- @
-- barrier ≡ fence >>= wait
-- @
barrier :: MonadTask m => m ()
barrier = fence >>= await

-- | Just run actions synchronously in IO
instance MonadTask IO where
  spawn = fmap return -- do it, do it now
  fence = Fence 0 <$> glFenceSync GL_SYNC_GPU_COMMANDS_COMPLETE 0
  await (Fence _ s) = go where
    go = glClientWaitSync s GL_SYNC_FLUSH_COMMANDS_BIT maxBound >>= \case
      GL_TIMEOUT_EXPIRED     -> go
      GL_WAIT_FAILED         -> throwErrors
      GL_ALREADY_SIGNALED    -> return ()
      GL_CONDITION_SATISFIED -> return ()
      e                      -> throw $ Error e

instance MonadTask m => MonadTask (ReaderT e m) where
  spawn (ReaderT m) = ReaderT $ \e -> lift `liftM` spawn (m e)
  fence = lift fence
  await = lift . await

-- * Synchronization point

type Continuation = Maybe Heap -> Int -> Int -> IO ()

-- * A simple non-empty pairing heap
data Heap = Heap !Fence Continuation [Heap]

mix :: Heap -> Heap -> Heap
mix x@(Heap i a as) y@(Heap j b bs)
  | i <= j    = Heap i a (y:as)
  | otherwise = Heap j b (x:bs)

merge :: [Heap] -> Maybe Heap
merge []     = Nothing
merge (x0:xs0) = Just (go x0 xs0) where
  go x (y:ys) = case ys of
    (z:zs) -> mix x y `mix` go z zs
    []     -> mix x y
  go x [] = x

push :: Fence -> Continuation -> Maybe Heap -> Heap
push f a (Just h) = mix (Heap f a []) h
push f a Nothing = Heap f a []

-- * OpenGL working thread task scheduler

newtype Task a = Task { runTask :: (a -> Continuation) -> Word64 -> IO () -> Continuation }
  deriving Functor

instance Applicative Task where
  pure = return
  (<*>) = ap

instance Monad Task where
  return a = Task $ \k _ _ -> k a
  Task m >>= f = Task $ \k d w -> m (\a -> runTask (f a) k d w) d w

-- await a given fence, idling when we timeout
drive :: Word64 -> IO () -> Heap -> Int -> Int -> IO ()
drive d w (Heap (Fence l s) k hs) i j 
  | l <= i       = k h i j -- we're already synchronized through to i. 
  | s == nullPtr = k h l j -- this is administrative, not a sync, l > i above
  | otherwise = go         -- use w >> go?
  where
    h = merge hs
    go = glClientWaitSync s 0 d >>= \case
      GL_TIMEOUT_EXPIRED     -> w >> go -- idle and try again
      GL_WAIT_FAILED         -> throwErrors
      GL_ALREADY_SIGNALED    -> k h l j
      GL_CONDITION_SATISFIED -> k h l j
      e                      -> throw $ Error e

instance MonadIO Task where
  liftIO m = Task $ \k _ _ h i j -> do
    a <- m 
    k a h i j 

instance MonadTask Task where
  fence = Task $ \k _ _ h i j -> do
    a <- glFenceSync GL_SYNC_GPU_COMMANDS_COMPLETE 0
    k (Fence j a) h i $! j + 1
  await f = Task $ \k d w mh -> drive d w $ push f (k ()) mh
{-
  spawn (Task m) = Task $ \k d w h i j -> do
    result <- newIORef Nothing
    let f = Fence j nullPtr
    let future h' i' j' = do
          m (\ a h'' i'' j'' -> writeIORef result a >> 
    k (await f) (push f (\h i' j' -> ) h
-}

-- |
-- @execTask delay idle task@ will run a task on the GPU and should
-- only be run in the bound thread that is controlling the user interface.
--
-- When we block waiting on a sync from the GPU and have no ready fibers then
-- we'll block for up to @delay@ nanoseconds and if that timeout expires, we'll run @idle@
execTask :: MonadIO m => Word64 -> IO () -> Task a -> m a
execTask d w (Task m) = liftIO $ do
  result <- newIORef Nothing
  m (\a h i j -> writeIORef result (Just a) >> pump h i j) d w Nothing 0 0
  readIORef result >>= maybe (fail "execTask: unfinished") return
 where
   pump :: Continuation
   pump Nothing _ _  = return ()
   pump (Just h) i j = drive d w h i j
