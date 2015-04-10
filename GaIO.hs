{-# LANGUAGE MultiParamTypeClasses #-}

-- =====================================================================
-- GaIO is the execution context for most Gitadmin IO
-- =====================================================================

module GaIO where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Error.Class (MonadError(..))

import Types
import GaErr
import GaCtx

newtype GaIO a = GaIO { unGaIO :: EitherT GaErr (ReaderT GaCtx IO) a }

instance Functor GaIO where
  fmap f ma = pure f <*> ma

instance Applicative GaIO where
  pure a = return a
  mf <*> ma = do { f <- mf; a <- ma; pure (f a); }

instance Monad GaIO where
  GaIO m >>= f = GaIO $ m >>= (unGaIO . f)
  return = GaIO . return

instance MonadIO GaIO where
  liftIO = GaIO . EitherT . lift . fmap Right

instance MonadError GaErr GaIO where
  throwError = GaIO . EitherT . return . Left
  catchError (GaIO ma) h =
   do ctx <- get_ctx
      r <- liftIO $ runReaderT (runEitherT ma) ctx
      case r of
          Left e  -> h e
          Right x -> GaIO (return x)

get_ctx :: GaIO GaCtx
get_ctx = GaIO $ lift ask

run_gaio :: GaCtx -> GaIO a -> IO ()
run_gaio ctx gaio = do
  r <- runReaderT (runEitherT (unGaIO gaio)) ctx
  case r of
    Left a -> putStrLn $ "Error: " ++ show a
    Right _ -> return ()
