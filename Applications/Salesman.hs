{-# LANGUAGE MultiParamTypeClasses, ParallelListComp, ViewPatterns, FlexibleInstances,TypeOperators #-}
import qualified Data.Map as M 
import Data.List
import System.IO.Unsafe
import Control.Arrow
import Control.Applicative
import Data.Ord
import System.Random
import System.Environment
import Control.Monad.State
import Control.Parallel.Strategies (rnf)
import Control.Concurrent


import Evolution
import Children 
import Genetics
import Lib.Distance
import Lib.Permutation
import Lib.Notes
import Populations

import Applications.SvgSalesman 

type Place = EuclideanIndividual -- we get Distance 
type Travel = Permutation Place -- we get Fitness and Children

-- next part should be semplified

type NotesForSalesman 	= (Fitness,Float) 
			:*: (Parents,ParentsSet) 
			:*: (Age,Int) 
			:*: (Insertions,[Gender (Permutation Place)]) 
			:*: ()

type ConfForSalesman 	= (LogRate,Int) 
			:*: (Parentado,Int) 
			:*: (CullChildren,Int) 
			:*: (Memory,Int) 
			:*: (CullPoint,Int) 
			:*: (Age,Int) 
			:*: ()

confGhost = undefined :: ConfForSalesman 

-- fitness using value of permutation
instance Fitted (Permutation Place) Float where
	fit x = 1 / value x
---
type Individual =  Noted (Permutation Place) NotesForSalesman 

isSolution (LogSolution x) = True
isSolution _ = False

mkImage :: [Permutation Place] -> String
mkImage = map toList >>> map (map (\(EuclideanIndividual [x,y]) -> (x,y))) >>> toSvg (1000,900)

run :: StdGen -> [Permutation Place] -> [EvolutionLog (Permutation Place)]
run g cs = snd . flip evalState g $ evolve (ConfigFile "data/salesman.conf", confGhost) step (map note cs :: [Individual]) where
	step xs = expand xs >>= lifeAndDeath >>= cullStep >>= \x -> pokeData x >> return x				

strictWriteFile s x = rnf x `seq` writeFile s x

main = do 	(read -> n):_ <- getArgs
		g  	<- newStdGen 
		g' 	<- newStdGen
		g'' 	<- newStdGen
		let 	cities =  fromList . map EuclideanIndividual $ 
				take n [[x,y] | x <- randomRs (100,1000) g | y <- randomRs (100,800) g'] :: Permutation Place
		 	rs = run g'' [cities]
		 	(ss,es) = partition isSolution $ rs
		forkIO (mapM_ (strictWriteFile "data/salesman.svg" . mkImage) . unfoldr (Just . splitAt 4) $ map (\(LogSolution x) -> x) $ ss)
		mapM_ print es



