{-# LANGUAGE DeriveGeneric #-}

module Cache (Cache, Function, Result, empty, lookupOrEval) where

import GHC.Generics
import qualified Data.Hashable as H
import qualified Data.HashMap.Lazy as HM
import Data.Maybe


newtype Cache k v = Cache (HM.HashMap k v)

-- name (String): function name
-- args: function arguments
data Function args = Function String args
    deriving (Eq, Generic)

instance (Show args) => Show (Function args) where
    show (Function name args) = name ++ " (" ++ show args ++ ")"

newtype Result result = Result result

instance (Show result) => Show (Result result) where
    show (Result result) = show result

instance (H.Hashable args) => H.Hashable (Function args)


empty :: Cache k v
empty = Cache HM.empty

lookupOrEval :: (Eq a, H.Hashable a) => Function a -> Cache (Function a) (Result r)
             -> (Result r, Cache (Function a) (Result r))
lookupOrEval f cache@(Cache hm)
  | isJust cached = (fromJust cached, cache)
  | otherwise = undefined -- evaluate and add to cache
  where cached = HM.lookup f hm
