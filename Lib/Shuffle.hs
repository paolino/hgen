-- | Random functions working with a rng under the State monad
{-# LANGUAGE FlexibleContexts, FlexibleInstances,ScopedTypeVariables #-}
module Lib.Shuffle where

import System.Random
import Control.Monad.State
import Data.Array.Diff
import Control.Arrow
import Control.Applicative

-- | A place holder for a monad with a StdGen in the state 
class (Functor m, MonadState StdGen m) => Randomized m 
instance Randomized (State StdGen)

-- | State version of randomR 
getRandomR :: (MonadState g m,RandomGen g,Random a,Functor m) 
	=> (a,a)	-- ^ bounds for the value
        -> m a		-- ^ picked value
getRandomR range = do
  (x,g) <- randomR range <$> get
  put g
  return x                         

-- | State version of randomRs
getRandomRs :: (MonadState g m,RandomGen g,Random a,Functor m) 
	=> (a,a)	-- ^ bounds for the value
	-> m [a]	-- ^ streams of picked values
getRandomRs range = do
  (g',g'') <- split <$> get
  put g''
  return $ randomRs range g'

picks :: (Functor m , MonadState g m,RandomGen g,Ord z,Random z,Num z)  
	=> [(z,a)]	-- ^ list of weighted elements (weight,elem)
        -> (z,z)    	-- ^ picking range !!
        -> m [a]        -- ^ picked elements
picks xs = (map j <$>) . getRandomRs where
	j x = snd. head. dropWhile ((< x). fst). scanl1 (\(s, _) (z, a) -> (s + z, a)) $ xs

picklinear :: Randomized m => [a] -> m [a]
picklinear xs = getRandomRs (0,length xs - 1) >>= return . map (xs !!)

pickonelinear :: Randomized m => [a] -> m a
pickonelinear xs = head <$> picklinear xs

pickby :: (Randomized m, Random b ,Num b, Ord b) => (a -> b) -> [a] -> m [a]
pickby f xs =  let 	fs = map f xs
		in picks (zip fs xs) (0,sum fs)

penalize :: forall a . [a -> Bool] -> [a] -> [[a]]
penalize bs = elems . foldr f z where
	f x as = accum (flip (:)) as [(length $ filter ($ x) bs, x)] 
	z = accumArray const []  (0, l) (zip [0..l] $ repeat []) :: DiffArray Int [a]
	l = length bs

pickvalidcouple :: forall a b m. Randomized m => ([a],[b])  -> (a -> b -> Bool) -> m (a,b)
pickvalidcouple (xs,ys) t = do	x <- pickonelinear xs
				y <- pickonelinear (head . filter (not . null) . penalize [t x] $ ys) :: m b
				return (x,y)


------ experimental code --------------------------------------


