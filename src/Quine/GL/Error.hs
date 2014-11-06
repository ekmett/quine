{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms #-}
module Quine.GL.Error
  ( Error(..)
  , pattern NoError
  , pattern InvalidEnum
  , pattern InvalidValue
  , pattern InvalidOperation
  , pattern InvalidFramebufferOperation
  , pattern OutOfMemory
  , pattern StackOverflow
  , pattern StackUnderflow
  , pattern TableTooLarge
  , showError
  , errors 
  , throwErrors
  , Errors(..)
  ) where

import Control.Exception hiding (StackOverflow)
import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import GHC.Generics
import Graphics.GL.Core45
import Graphics.GL.Ext.ARB.Imaging
import Graphics.GL.Types

-- | Used to represent the result of 'glGetError'
newtype Error = Error GLenum deriving (Typeable, Data, Generic)

-- | No error has been recorded. 
pattern NoError = Error GL_NO_ERROR

-- | An unacceptable value is specified for an enumerated argument.
pattern InvalidEnum = Error GL_INVALID_ENUM

-- | A numeric argument is out of range.
pattern InvalidValue = Error GL_INVALID_VALUE

-- | The specified operation is not allowed in the current state. 
pattern InvalidOperation = Error GL_INVALID_OPERATION

-- | The framebuffer object is not complete.
pattern InvalidFramebufferOperation = Error GL_INVALID_FRAMEBUFFER_OPERATION

-- | There is not enough memory left to execute the command.
pattern OutOfMemory = Error GL_OUT_OF_MEMORY

-- | This command would cause a stack overflow.
pattern StackOverflow = Error GL_STACK_OVERFLOW

-- | This command would cause a stack underflow.
pattern StackUnderflow = Error GL_STACK_UNDERFLOW

-- | The specified table exceeds the implementation's maximum supported table size.
--
-- Used by @Graphics.GL.Raw.Exension.ARB.Imaging@ and @Graphics.GL.Raw.EXT.Histogram@
pattern TableTooLarge = Error GL_TABLE_TOO_LARGE

-- | Generate a user-friendly version of an OpenGL error.
showError :: Error -> String
showError NoError                     = "no error"
showError InvalidEnum                 = "invalid enumerated argument"
showError InvalidValue                = "numeric argument out of range"
showError InvalidOperation            = "operation not allowed in the current state"
showError InvalidFramebufferOperation = "framebuffer object not complete"
showError StackOverflow               = "stack overflow"
showError StackUnderflow              = "stack underflow"
showError OutOfMemory                 = "out of memory"
showError TableTooLarge               = "table too large"
showError (Error e)                   = "unknown error " ++ show e

instance Show Error where
  showsPrec d (Error e) = case e of
    GL_NO_ERROR -> showString "NoError"
    GL_INVALID_ENUM -> showString "InvalidEnum"
    GL_INVALID_VALUE -> showString "InvalidValue"
    GL_INVALID_OPERATION -> showString "InvalidOperation"
    GL_INVALID_FRAMEBUFFER_OPERATION -> showString "InvalidFramebufferOperation"
    GL_OUT_OF_MEMORY -> showString "OutOfMemory"
    GL_STACK_OVERFLOW -> showString "StackOverflow"
    GL_STACK_UNDERFLOW -> showString "StackUnderflow"
    GL_TABLE_TOO_LARGE -> showString "TableTooLarge"
    _ -> showParen (d > 10) $ showString "Error " . showsPrec 11 e

instance Exception Error

newtype Errors = Errors [Error] deriving (Show, Typeable, Data, Generic)

instance Exception Errors

-- | No error in the result list should be 'NoError'!
errors :: MonadIO m => m [Error]
errors = liftIO $ go [] where
  go es = do
    e <- glGetError
    if e == GL_NO_ERROR 
      then return $ reverse es
      else go $ Error e:es

-- | Throw 'Errors' if we have any
throwErrors :: MonadIO m => m ()
throwErrors = do
  es <- errors
  unless (null es) $ throw $ Errors es
