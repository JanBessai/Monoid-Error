import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import Data.Monoid

newtype ErrorPlusT e m a = ErrorPlusT { runErrorPlusT :: m (Either e a) }

instance (Monad m) => Monad (ErrorPlusT e m) where
  return = ErrorPlusT . return . Right
  (ErrorPlusT x) >>= f = ErrorPlusT $ x >>= either (return . Left) (runErrorPlusT . f)

instance (Error e, Monad m) => MonadError e (ErrorPlusT e m) where
  throwError = ErrorPlusT . return . Left
  catchError (ErrorPlusT x) f = ErrorPlusT $ x >>= either (runErrorPlusT . f) (return . Right)

instance (Monad m, Monoid e, Error e) => MonadPlus (ErrorPlusT e m) where
  mzero = ErrorPlusT . return . Left $ mempty
  mplus x y = catchError x (\ err -> catchError y (ErrorPlusT . return . Left . mappend err))

instance (Functor m) => Functor (ErrorPlusT e m) where
  fmap f =  ErrorPlusT . fmap (fmap f) . runErrorPlusT

instance (Applicative m) => Applicative (ErrorPlusT e m) where
  pure = ErrorPlusT . pure . Right
  (ErrorPlusT f) <*> (ErrorPlusT x) = 
    ErrorPlusT (either (const . Left) (\ f -> either (Left) (Right . f)) <$> f <*> x)

instance (Monoid e, Applicative m) => Alternative (ErrorPlusT e m) where
  empty = ErrorPlusT . pure . Left $ mempty
  (ErrorPlusT x) <|> (ErrorPlusT y) = 
    ErrorPlusT $ either (\ err -> either (Left . mappend err) (Right)) (const . Right) <$> x <*> y

instance MonadTrans (ErrorPlusT e) where
  lift m = ErrorPlusT $ m >>= (return . Right)

instance (MonadFix m) => MonadFix (ErrorPlusT e m) where
  mfix f = ErrorPlusT $ mfix (either (return . Left) (runErrorPlusT . f))

instance (Error e, MonadIO m) => MonadIO (ErrorPlusT e m) where
  liftIO = lift . liftIO

instance (MonadState s m) => MonadState s (ErrorPlusT e m) where
  get = lift get
  put = lift . put