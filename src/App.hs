module App (
    run,
    query_,
    fromEither,
    fromEitherWith,
    executeWithVisit,
) where

import Control.Monad.IO.Class

import Control.Exception.Safe
import Control.Monad.Trans.Except
import qualified Database.SQLite.Simple as SL

import Types

runDB :: (MonadIO m) => Maybe Visit -> IO a -> App m a
runDB v = withExceptT (Db v . displayException) . ExceptT . liftIO . tryAny

query_ :: (MonadIO m, SL.FromRow r) => SL.Connection -> SL.Query -> App m [r]
query_ c q = runDB Nothing (SL.query_ c q)

executeWithVisit :: (MonadIO m, SL.ToRow i) => Visit -> SL.Connection -> SL.Query -> i -> App m ()
executeWithVisit v c q r = runDB (Just v) (SL.execute c q r)

fromEither :: (Applicative m) => Either Err a -> App m a
fromEither = ExceptT . pure

fromEitherWith :: (Applicative m) => (String -> Err) -> Either String a -> App m a
fromEitherWith e = withExceptT e . ExceptT . pure

run :: App m a -> m (Either Err a)
run = runExceptT
