module List ((++), (!!), cons, head, last, tail, init, length, take, drop, empty, null, snoc, reverse) where

import Prelude hiding ((++), (!!), head, last, tail, init, length, take, drop, empty, null, reverse)
import qualified Prelude as P
import qualified Data.IntMap as IntMap
import Test.QuickCheck ((==>), quickCheck, Property)


newtype List a = List {
        list :: IntMap.IntMap a
                      }
    deriving Eq

instance (Show a) => Show (List a) where
    show (List l)
      | IntMap.null l = "[]"
      | otherwise = '[' : P.drop 1 (IntMap.foldr (\e str -> ',' : show e P.++ str) "]" l)

prop_show :: (Show a) => [a] -> Bool
prop_show l =
    show l == show (fromWrongList l)


fromWrongList :: [a] -> List a
fromWrongList = List . IntMap.fromList . zip [0..]

toWrongList :: List a -> [a]
toWrongList = IntMap.elems . list

prop_convert :: (Eq a) => [a] -> Bool
prop_convert l =
    l == toWrongList (fromWrongList l)

-- increment all indices
shiftBy :: Int -> List a -> List a
shiftBy n = List . IntMap.mapKeys (+ n) . list

shift :: List a -> List a
shift = shiftBy 1


empty :: List a
empty = List IntMap.empty

prop_empty :: Bool
prop_empty = P.null (toWrongList empty) && null (fromWrongList [])

null :: List a -> Bool
null = IntMap.null . list

prop_null :: Bool
prop_null = null $ fromWrongList []

cons :: a -> List a -> List a
cons a = List . IntMap.insert 0 a . list . shift

prop_cons :: (Eq a) => a -> [a] -> Bool
prop_cons a l =
    (a:l) == toWrongList (cons a (fromWrongList l))

snoc :: List a -> a -> List a
snoc list@(List l) a = List $ IntMap.insert (length list) a l

prop_snoc :: (Eq a) => [a] -> a -> Bool
prop_snoc l v =
    l P.++ [v] == toWrongList (snoc (fromWrongList l) v)

(++) :: List a -> List a -> List a
(++) (List l1) = List . IntMap.union l1 . list . shiftBy (IntMap.size l1)

prop_append :: (Eq a) => [a] -> [a] -> Bool
prop_append a b =
    a P.++ b == toWrongList (fromWrongList a ++ fromWrongList b)

(!!) :: List a -> Int -> a
(!!) (List l) = (IntMap.!) l

prop_ind :: (Eq a) => Int -> [a] -> Property
prop_ind n l =
    n >= 0 && n < P.length l ==> l P.!! n == fromWrongList l !! n

head :: List a -> a
head = snd . IntMap.findMin . list

prop_head :: (Eq a) => [a] -> Property
prop_head l =
    (not . P.null) l ==> P.head l == head (fromWrongList l)

last :: List a -> a
last = snd . IntMap.findMax . list

prop_last :: (Eq a) => [a] -> Property
prop_last l =
    (not . P.null) l ==> P.last l == last (fromWrongList l)

tail :: List a -> List a
tail = List . IntMap.deleteMin . list

prop_tail :: (Eq a) => [a] -> Property
prop_tail l =
    (not . P.null) l ==> P.tail l == toWrongList (tail (fromWrongList l))

init :: List a -> List a
init = List . IntMap.deleteMax . list

prop_init :: (Eq a) => [a] -> Property
prop_init l =
    (not . P.null) l ==> P.init l == toWrongList (init (fromWrongList l))

length :: List a -> Int
length = IntMap.size . list

prop_length :: [a] -> Bool
prop_length l =
    P.length l == length (fromWrongList l)

take :: Int -> List a -> List a
take n = List . fst . IntMap.split n . list

prop_take :: (Eq a) => Int -> [a] -> Bool
prop_take n l =
    P.take n l == toWrongList (take n (fromWrongList l))

drop :: Int -> List a -> List a
drop n = List . snd . IntMap.split (pred n) . list

prop_drop :: (Eq a) => Int -> [a] -> Bool
prop_drop n l =
    P.drop n l == toWrongList (drop n (fromWrongList l))

reverse :: List a -> List a
reverse = List . snd . IntMap.foldr (\v (i,m) -> (i + 1, IntMap.insert i v m)) (0, list empty) . list

prop_reverse :: (Eq a) => [a] -> Bool
prop_reverse l =
    P.reverse l == toWrongList (reverse (fromWrongList l))

{-
infList :: (Enum a) => a -> List a
infList start = infListStep start succ

infListStep :: (Enum a) => a -> (a -> a) -> List a
infListStep start step = cons start $ infListStep (step start) step
-}

infList' :: (Enum a) => a -> List a
infList' start = infListStep' start succ

infListStep' :: (Enum a) => a -> (a -> a) -> List a
infListStep' start step = infListWhile start step (const True)

infListWhile :: (Enum a) => a -> (a -> a) -> (a -> Bool) -> List a
infListWhile start step cond = reverse $ infListWhile' start step cond
    where infListWhile' :: (Enum a) => a -> (a -> a) -> (a -> Bool) -> List a
          infListWhile' start step cond
            | cond start = snoc (infListWhile' (step start) step cond) start
            | otherwise = empty

prop_infListWhile :: Int -> Int -> Property
prop_infListWhile start stop =
    start <= stop ==> [start..stop] == toWrongList (infListWhile start succ (<= stop))
