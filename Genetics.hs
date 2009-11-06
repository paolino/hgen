{-# LANGUAGE 	ExistentialQuantification, MultiParamTypeClasses, FlexibleContexts, 
 FlexibleInstances, ScopedTypeVariables, ViewPatterns, FunctionalDependencies, UndecidableInstances #-}

-- | A module implementing an algorithm for genetic search on simple genomes 
module Genetics  
	where

import Data.List (sortBy, intersect, null,transpose)
import Control.Applicative ((<$>))
import Data.Ord (comparing)
import Control.Monad.RWS (ask,tell,RWS)
import System.Random
import Control.Parallel.Strategies
----------------------------------------------------------------------------------------------------------

import Evolution (Core,EvolutionLog (..))
import Lib.Notes 
import Lib.Counter 
import Lib.Shuffle
import Lib.Permutation
import Lib.Distance

-- | a particular  of a world changing
type Change m a = a -> m a

-------------- Fitness conceptuals -----------------------------------------------------------------------
-- | quality for something
data Fitness = Fitness deriving Read
instance NFData Fitness

-- | the class to implement for a something to have value associated
class Fitted c b | c -> b where
	fit :: c -> b


-- the fitness note creator for a Fitted value
instance (Num b,Fitted c b) => HasNote c (Fitness,b) where
	note x = (Fitness, fit x)

-------------------- Aging operator -----------------------------------------------------------------
-- | holding an Age, used as configuration and annotation of a gene
data Age = Age deriving Read
instance NFData Age
-- | no matter what the gene is , it starts with 0 age
instance HasNote c (Age,Int) where
	note _ = (Age,0)
		
-- | Dropping too old organisms
lifeAndDeath :: (Label Age Int cf, Label Age Int ds) =>  Change (Core cf c) [Noted c ds]
lifeAndDeath ps = do 	max <- look Age <$> ask 
			return . filter ((< max) . look Age . notes) . map (lookchange Age (+1)) $ ps
-----------------------------------------------------------------------------------
-- | A cutting point for a sortable by fitness population
data CullPoint = CullPoint deriving Read

-- | the standard sort and cut  tool for something noted with a fitness, using the Cull conf 
cullStep :: forall b cf ds  c. (
	Ord b , 		-- ^ fitness must be sortable
	Label CullPoint Int cf,  -- ^ a configuration for culling
	Label Fitness b ds	-- ^ people has fitness
	) 
	=> Change (Core cf c) [Noted c ds]
cullStep ps = do	n <- look CullPoint <$> ask
			return . take n . sortBy (\x y -> comparing (look Fitness) y x) $ ps
----------------------------------------------------------------------------
-- | Output configuration and operator
data LogRate = LogRate deriving Read

pokeData ::( Show c, Label LogRate Int cf) => [Noted c ds] -> Core cf c ()
pokeData ps = look LogRate <$> ask >>= \n -> (tell . map (LogSolution . datum)  .take n) ps 
-----------------------------------------------------------------------------

-- | the level of parentado to keep from parents by a child
data Parentado = Parentado deriving Read

-- | Parents as Set of Identifiers, wrong structure, use also a list for the order !
data Parents = Parents
instance NFData Parents

type ParentsSet = [Int]

-- | care! after this is called the set must be filled from the children creator
-- this is here for when a datum is there for no parental reason
instance HasNote c (Parents,ParentsSet) where
	note _ = (Parents, [counter 1]) 

-- | test if this are notes of non parented stuff
parentOracle :: (Label Parents ParentsSet n1, Label Parents ParentsSet n2) => n1 -> n2 -> Bool
parentOracle (look Parents -> s1) (look Parents -> s2) = null (intersect s1 s2) where

-- | computation of new parents for a child, using father and mother parents (broken!)
newParents :: (	Label Parentado Int cf,
		Label Parents ParentsSet n1, 
		Label Parents ParentsSet n2) 
		=> n1 -> n2 -> Change (Core cf c) ParentsSet
newParents (look Parents -> s1) (look Parents -> s2) p = do
	dpt <- look Parentado <$> ask
	return . (p ++) . concat . transpose . map (take (2 ^ dpt - 1)) $ [s1,s2]


