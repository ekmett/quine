module Quine.Supply
  ( Supply
  , HasSupply(..)
  , split
  , fresh
  ) where

import Control.Concurrent.Supply
import Control.Lens
import Control.Monad.State.Class

class HasSupply s where
  supply :: Lens' s Supply

instance HasSupply Supply where
  supply = id

-- | Split off a variable supply that will return distinct identifiers
-- from those returned by the current supply.
split :: (MonadState s m, HasSupply s) => m Supply
split = supply %%= splitSupply

-- | Get a globally unique fresh id, so long as every 'Supply' is accessed
-- by a single thread, and none leak 'reused' keys.
fresh :: (MonadState s m, HasSupply s) => m Int
fresh = supply %%= freshId
