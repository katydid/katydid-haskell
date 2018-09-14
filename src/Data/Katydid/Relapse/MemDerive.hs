-- |
-- This module is an efficient implementation of the derivative algorithm for trees.
--
-- It is intended to be used for production purposes.
--
-- This means that it gives up some readability for speed.
--
-- This module provides memoization of the nullable, calls and returns functions.

module Data.Katydid.Relapse.MemDerive
  ( derive
  , Mem
  , newMem
  , validate
  )
where

import qualified Data.Map.Strict               as M
import           Control.Monad.State            ( State
                                                , runState
                                                , lift
                                                , state
                                                )
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , runExceptT
                                                )

import           Data.Katydid.Parser.Parser

import qualified Data.Katydid.Relapse.Derive   as Derive
import           Data.Katydid.Relapse.Smart     ( Grammar
                                                , Pattern
                                                , lookupRef
                                                , nullable
                                                , lookupMain
                                                )
import           Data.Katydid.Relapse.IfExprs
import           Data.Katydid.Relapse.Expr
import           Data.Katydid.Relapse.Zip

mem :: Ord k => (k -> v) -> k -> M.Map k v -> (v, M.Map k v)
mem f k m | M.member k m = (m M.! k, m)
          | otherwise    = let res = f k in (res, M.insert k res m)

type Calls = M.Map [Pattern] IfExprs
type Returns = M.Map ([Pattern], [Bool]) [Pattern]

-- |
-- Mem is the object used to store memoized results of the nullable, calls and returns functions.
newtype Mem = Mem (Calls, Returns)

-- |
-- newMem creates a object used for memoization by the validate function.
-- Each grammar should create its own memoize object.
newMem :: Mem
newMem = Mem (M.empty, M.empty)

calls :: Grammar -> [Pattern] -> State Mem IfExprs
calls g k = state $ \(Mem (c, r)) ->
  let (v', c') = mem (Derive.calls g) k c in (v', Mem (c', r))

returns :: Grammar -> ([Pattern], [Bool]) -> State Mem [Pattern]
returns g k = state $ \(Mem (c, r)) ->
  let (v', r') = mem (Derive.returns g) k r in (v', Mem (c, r'))

mderive
  :: Tree t
  => Grammar
  -> [Pattern]
  -> [t]
  -> ExceptT String (State Mem) [Pattern]
mderive _ ps []          = return ps
mderive g ps (tree : ts) = do
  ifs                <- lift $ calls g ps
  childps            <- hoistExcept $ evalIfExprs ifs (getLabel tree)
  (zchildps, zipper) <- return $ zippy childps
  childres           <- mderive g zchildps (getChildren tree)
  let nulls   = map nullable childres
      unzipns = unzipby zipper nulls
  rs <- lift $ returns g (ps, unzipns)
  mderive g rs ts

hoistExcept :: (Monad m) => Either e a -> ExceptT e m a
hoistExcept = ExceptT . return

-- |
-- derive is the classic derivative implementation for trees.
derive :: Tree t => Grammar -> [t] -> Either String Pattern
derive g ts =
  let start    = [lookupMain g]
      (res, _) = runState (runExceptT $ mderive g start ts) newMem
  in  case res of
        (Left  l  ) -> Left l
        (Right [r]) -> return r
        (Right rs ) -> Left $ "not a single pattern: " ++ show rs

-- |
-- validate is the uses the derivative implementation for trees and
-- return whether tree is valid, given the input grammar and start pattern.
validate :: Tree t => Grammar -> Pattern -> [t] -> (State Mem) Bool
validate g start tree = do
  rs <- runExceptT (mderive g [start] tree)
  return $ case rs of
    (Right [r]) -> nullable r
    _           -> False
