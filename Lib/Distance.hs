{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances, UndecidableInstances  #-}
module Lib.Distance where
import Data.Map as M
import Data.List
------------------------------------------------ Distances --------------------------------
class Num b  => Distance a b | a -> b  where
	dist :: a -> a -> b


dista cachedist x y = case M.lookup (x,y) cachedist of 
		Just l -> l
		Nothing -> case M.lookup (y,x) cachedist of
			Just l -> l
			Nothing -> error ("Out of cachedist lookup for " ++ show x ++ "," ++ show y)

mkCacheDistance xs = M.fromList [((x,y),dist x y) | (x:xs') <- init . init . tails $ xs, y <- xs']

------------------------------------------------------------------------------------------

instance Distance Double Double where
	dist x y = abs (x - y)
instance Distance Float Float where
	dist x y = abs (x - y)
