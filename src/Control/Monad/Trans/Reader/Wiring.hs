module Control.Monad.Trans.Reader.Wiring(
  promoteReader
) where

import Control.Monad.Wiring
import Control.Monad.Trans.Reader

-- $setup
-- >>> import Control.Monad.Wiring
-- >>> import Data.Functor.Identity
-- >>> data Database1 = Database1
-- >>> data Database2 = Database2
-- >>> let request1 userId = ReaderT $ (\database -> return ("User" ++ userId)) :: ReaderT Database1 Identity String
-- >>> let request2 userId = ReaderT $ (\database -> return ("User" ++ userId)) :: ReaderT Database2 Identity String

-- | Promote from one ReaderT instance to another.
--
-- >>> :{
-- let composedResult = do
--    result1 <- promoteReader $ request1 "1"
--    result2 <- promoteReader $ request2 "2"
--    return [result1, result2] :: ReaderT (Database1, Database2) Identity [String]
-- :}
-- >>> runIdentity $ runReaderT composedResult (Database1, Database2)
-- ["User1", "User2"]
promoteReader :: (Wirable r1 r2) => ReaderT r2 m a -> ReaderT r1 m a
promoteReader = withReaderT wire
