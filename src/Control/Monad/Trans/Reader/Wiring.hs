module Control.Monad.Trans.Reader.Wiring(
  promoteReader
) where

import Control.Monad.Wiring
import Control.Monad.Trans.Reader

-- | Promote from one ReaderT instance to another.
-- $setup
-- >>> import Control.Monad.Wiring
-- >>> data Database1 = Database1
-- >>> data Database2 = Database2
-- >>> let request1 userId = ReaderT $ (\database -> return $ ("User" ++ userId)) :: ReaderT Database1 IO String
-- >>> let request2 userId = ReaderT $ (\database -> return $ ("User" ++ userId)) :: ReaderT Database2 IO String

-- >>> let composedResult = do
-- >>>   result1 <- promoteReader $ request1 "1"
-- >>>   result2 <- promoteReader $ request2 "2"
-- >>>   return $ putStrLn "Test"
-- >>>   return [result1, result2] :: ReaderT (Database1, Database2) IO String
-- >>> runReaderT composedResult (Database1, Database2)
-- ["User1", "User2"]
promoteReader :: (Wirable r1 r2) => ReaderT r2 m a -> ReaderT r1 m a
promoteReader = withReaderT wire
