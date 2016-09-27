module Data.FunctionalList
where

import           Prelude (Bool(..), (.), (++), undefined)
import qualified Prelude as P

type List a = [a] -> [a]

-- ----------------------------------------
--toList . fromList = id
-- fromlist (tolist xs) = xs

-- holt performance raus durch Funktionskomposition in konstanter Laufzeit mit anschließender Konvertierung zu einer "normalen" Liste
-- Parameter können aus Funktionen gekürzt werden

fromList        :: [a] -> List a
fromList l      = \ ys -> l ++ ys

toList          :: List a -> [a]
toList l        = l []

empty           :: List a
empty           = P.id

singleton       :: a -> List a
-- singleton       = (:)
singleton e  = \xs -> e : xs
-- (:) for functional lists
cons            :: a -> List a -> List a
--cons e l        = \xs -> e : l xs
--cons e l = \ xs -> (e:).l $ xs
cons e l = singleton e . l
-- dual to cons
snoc            :: List a -> a -> List a
snoc l e        = l . singleton e

-- (++) for functional lists
append          :: List a -> List a -> List a
--append l1 l2    = \xs -> l1 (l2 xs)
--append l1 l2 = l1 . l2
append = (.)
-- like concat for normal lists: foldr (++) []
concat          :: [List a] -> List a
concat          = P.foldr append empty

-- like map for normal lists: foldr ((:) . f) []
map             :: (a -> b) -> List a -> List b
map f           = P.map f ++ . toList

-- foldr with foldr for normal lists
foldr           :: (a -> b -> b) -> b -> List a -> b
foldr op n      = P.fold op n . toList

-- head, tail, null
head            :: List a -> a
head            = P.head . toList

tail            :: List a -> List a
tail            = P.tail . toList

null            :: List a -> Bool
null            = P.null . toList

reverse         :: List a -> List a
reverse         = P.reverse . toList

-- ----------------------------------------
