module UnsafeMem
    ( memoize
    ) where

import Prelude hiding (lookup)
import Data.Map (empty, lookup, insert)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do 
    r <- newIORef empty
    return $ \ x -> unsafePerformIO $ do 
        m <- readIORef r
        case lookup x m of
            Just y  -> return y
            Nothing -> do 
                    let y = f x
                    writeIORef r (insert x y m)
                    return y
