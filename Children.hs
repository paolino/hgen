{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, ScopedTypeVariables,
 FunctionalDependencies, UndecidableInstances,ViewPatterns #-}

-- | Module to use for population expansion under the Children class.
--
module Children (
	Children (..),
	CullChildren (..),
	Insertions (..),
	expand,
	Memory (..),
	Gender (..)
	) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad.RWS (ask,replicateM)
import System.Random
import Data.Array.Diff
import Data.Ord (comparing)
import Data.List (sortBy, group, sort, transpose, inits, tails)
import Control.Parallel.Strategies (NFData, rnf)

import Lib.Notes
import Lib.Shuffle (pickby, picklinear, penalize, Randomized, pickonelinear,getRandomR,pickvalidcouple)
import Lib.Permutation 
import Lib.Distance 

import Evolution (Core)
import Genetics

-- | Marker for aplodi
data Gender a = Male {aploide :: a} | Female {aploide :: a} deriving Show

instance NFData a => NFData (Gender a) where
	rnf = rnf . aploide

sameGender (Male _) (Male _) = True
sameGender (Female _) (Female _) = True
sameGender _ _ = False

instance Functor Gender where
	fmap f g = g {aploide = f (aploide g)}

-- | necessary if you want to use expand, note c are sortable by their fitness
class Children c d | c -> d  where
	child 	:: Randomized m 	-- ^ with a StdGen in the state
		=> (Gender d,Gender d) 	-- ^ A couple of insertions (should be different in Gender)
		-> m c	-- ^ a new gene with its new insertions
	aploidi :: Randomized m => c -> m [Gender d]

-- | label for the genetic memory 
data Insertions = Insertions

instance NFData Insertions

-- | the type of insertions
type InsertionsList d = [Gender d]

-- | default annotations values for Insertions 
instance Children c d => HasNote c (Insertions,InsertionsList d) where
	note x = (Insertions, [])

-- | take care the element is not an ancestor with uninitiated insertions
careInsertion :: (Label Insertions (InsertionsList d) ls, Children c d, Randomized m ) => Noted c ls -> m (InsertionsList d)
careInsertion x = let is = look Insertions x in if null is then aploidi (datum x) else return is

-- | label for configuration, holding the number of generation to store in each individual memory
data Memory = Memory deriving Read


-- | making a new gene from the population
new 	:: forall c cf d ls .(
	Children c d,
	Label Parents ParentsSet  ls,
	Label Insertions (InsertionsList d) ls,
	Label Memory Int cf,
	Label Parentado Int cf,
	c `HasNote` ls
	)
	=> [Noted c ls] 		-- ^ Some genes
	-> Core cf c (Noted c ls)	-- ^ The new gene
new ps = do 	(x,y) 	<- pickvalidcouple (ps,ps) parentOracle
		dsx <- careInsertion x
		dsy <- careInsertion y
		r 	<- look Memory <$> ask
		let	ms = take (2* (2 ^ r - 1)) . concat . transpose $ [dsx,dsy]
		(d1,d2)	<- pickvalidcouple (dsx,dsy) sameGender
		z 	<- child (d1,d2) :: Core cf c c
		ds 	<- aploidi z :: Core cf c ([Gender d])
		lookchangeM Parents (newParents x y) (change (Insertions, ds ++ ms) $ note z) 

-- | How many children to keep each expansion
data CullChildren = CullChildren deriving Read 

-- | the function to expand a population by mixing genes, the output population is not sorted
expand 	:: forall c d cf ls . (
	Children c d , 
	c `HasNote` ls,
	Label Insertions (InsertionsList d) ls, 
	Label Parents ParentsSet	ls,
        Label Parentado Int cf,
	Label Memory Int cf,
	Label CullChildren Int cf
	)
	=> Change (Core cf c) [Noted c ls]	-- ^ a mutation population operator
expand ps = do	n <- look CullChildren <$> ask
		(ps ++) <$> replicateM n (new ps)


-------------------  instances of Children ------------------------------------------------


instance forall c b . (NFData c, Eq c, Ord b, Distance c b) => Children (Permutation c) (Permutation c) where
	child (Female df, Male dm) = return $ bestcouple (df \\ dm) dm 
	child (Male d, Female c) = child (Female c, Male d)
	child _	 = error "Permutation : Cannot mix same gender inserts"
	aploidi c = do 	n <- getRandomR (0,len c -1)
			t <- getRandomR (1,len c -1)
			return [Male (apiece n t c), Female c]

	
