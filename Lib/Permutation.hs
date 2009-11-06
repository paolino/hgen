{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

module Lib.Permutation (Permutation,len,apiece,fromList,toList,bestcouple,(\\),value) where

import Data.List (tails)
import Control.Arrow ((***))
import Control.Monad.State
import Control.Applicative
import Control.Arrow ((>>>),(***),(&&&))
import Control.Parallel.Strategies

import qualified Data.List as List (minimumBy,(\\))
import Data.Ord (comparing)
import Data.Function (on)

import Lib.Distance
import Lib.Shuffle

----------------- structurals ------------------------------------
data Permutation a = PermutationData {len ::Int, right ::[a], left :: [a]} 
instance Eq a => Eq (Permutation a) where
	(==) = ((==) `on` toList) 

instance Show a => Show (Permutation a) where
	show = show . toList
fromList :: [a] -> Permutation a
fromList xs = let l = length xs in
	PermutationData (length xs) (cycle xs)  (cycle (reverse xs))

toList :: Permutation a -> [a]
toList = take . len  <*> right 

shift :: Int -> Permutation a -> Permutation a
shift m c@(PermutationData l xs ys) 
	| m == 0	= c
	| m > 0 	= let (xs',ys') = transfer (mod m l) (xs,ys) in 
		PermutationData l xs' ys'
	| m < 0		= let (ys',xs') = transfer (negate $ mod m l) (ys,xs) in 
		PermutationData l xs' ys'
	where
	transfer n = (!! n) . iterate move  
	move (x:xs,ys) = (xs,x:ys)

rotations = take . len <*> iterate (shift 1)

reflections t@(PermutationData l xs ys) = [t,PermutationData l ys xs]

couple (PermutationData l xs ys) (PermutationData l' xs' ys') = PermutationData (l + l') 
	(cycle $ take l xs ++ take l' xs')
	(cycle $ take l' ys' ++ take l ys) 

c \\ d = fromList $ toList c List.\\ toList d

instance NFData a => NFData (Permutation a) where
	rnf (PermutationData l xs ys) = rnf (l,(take l xs),(take l ys))

------------------- distance related ------------------

diff :: Distance a b => [a] -> [a] -> b
diff xs ys = sum $ zipWith dist xs ys

cutvalue :: Distance a b => Permutation a -> b
cutvalue (PermutationData _  (x:_) (y:_)) = dist x y

couplevalue :: Distance a b => Permutation a -> Permutation a -> b
couplevalue (PermutationData _ (x:_) (y:_)) (PermutationData _ (x':_) (y':_)) = dist x y' + dist x' y

value x = diff (toList x) (toList $ shift 1 x)

instance (Eq a, Ord b, Distance a b) => Ord (Permutation a) where
	compare = comparing value

bestcouple :: forall a b . (Ord b, Distance a b) => Permutation a -> Permutation a -> Permutation a
bestcouple c d  = couple x y where 
	v x y = couplevalue x y - cutvalue x
	(_,(x,y)) = List.minimumBy (comparing fst) . map (uncurry v  &&& id) $ [(x,y) | x <- rotations c , y <- reflections d]

apiece n t c = fromList $ take t . toList $ shift n c
 ------------ Example ------------------
--
--instance Distance Int Int where
--	dist x y = abs (x - y)
-- bestcouple (fromList [


