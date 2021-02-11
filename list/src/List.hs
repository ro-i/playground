module List ((++), cons, head, last, tail, init, length) where

import Prelude hiding ((++), head, last, tail, init, length)
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
      | otherwise = '[' : drop 1 (IntMap.foldr (\e str -> ',' : show e P.++ str) "]" l)

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


cons :: a -> List a -> List a
cons a = List . IntMap.insert 0 a . list . shift

prop_cons :: (Eq a) => a -> [a] -> Bool
prop_cons a l =
    (a:l) == toWrongList (cons a (fromWrongList l))

(++) :: List a -> List a -> List a
(++) (List l1) = List . IntMap.union l1 . list . shiftBy (IntMap.size l1)

prop_append :: (Eq a) => [a] -> [a] -> Bool
prop_append a b =
    a P.++ b == toWrongList (fromWrongList a ++ fromWrongList b)

head :: List a -> a
head = snd . IntMap.findMin . list

prop_head :: (Eq a) => [a] -> Property
prop_head l =
    (not . null) l ==> P.head l == head (fromWrongList l)

last :: List a -> a
last = snd . IntMap.findMax . list

prop_last :: (Eq a) => [a] -> Property
prop_last l =
    (not . null) l ==> P.last l == last (fromWrongList l)

tail :: List a -> List a
tail = List . IntMap.deleteMin . list

prop_tail :: (Eq a) => [a] -> Property
prop_tail l =
    (not . null) l ==> P.tail l == toWrongList (tail (fromWrongList l))

init :: List a -> List a
init = List . IntMap.deleteMax . list

prop_init :: (Eq a) => [a] -> Property
prop_init l =
    (not . null) l ==> P.init l == toWrongList (init (fromWrongList l))

length :: List a -> Int
length = IntMap.size . list

prop_length :: [a] -> Bool
prop_length l =
    P.length l == length (fromWrongList l)
