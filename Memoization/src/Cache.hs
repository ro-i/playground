{-# LANGUAGE DeriveGeneric #-}

module Cache where

import GHC.Generics
import qualified Data.Hashable as H
import qualified Data.HashMap.Lazy as HM
import Data.Maybe


newtype Cache k v = Cache (HM.HashMap k v)

-- name (String): function name
-- args: function arguments
-- result: result of this function applied to this arguments.
data Entry args result = Entry String args result
    deriving (Eq, Generic)

instance (Show args, Show result) => Show (Entry args result) where
    show (Entry name args result) =
        name ++ " (" ++ show args ++ ") :" ++ show result

instance (H.Hashable args, H.Hashable result) => H.Hashable (Entry args result)


empty :: Cache k v
empty = Cache HM.empty

lookupOrEval :: (Eq k, H.Hashable k) => k -> Cache k v -> (v, Cache k v)
lookupOrEval k cache@(Cache hm)
  | isJust cached = (fromJust cached, cache)
  | otherwise = undefined -- evaluate and add to cache
  where cached = HM.lookup k hm
