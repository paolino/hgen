{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
module Populations  where
import Text.ParserCombinators.ReadP
import Lib.Distance
import qualified Data.Map as M
import Control.Arrow
import Control.Applicative
import Data.Char
import Data.List
import Control.Parallel.Strategies

instance Distance EuclideanIndividual Float where
	dist (EuclideanIndividual x) (EuclideanIndividual y) = sqrt . sum $ zipWith dist x y 

newtype EuclideanIndividual = EuclideanIndividual [Float] deriving (Show,Eq)
instance NFData EuclideanIndividual where
	rnf (EuclideanIndividual x) = rnf x

newtype EuclideanPopulation = EuclideanPopulation [EuclideanIndividual] deriving Show


instance Read EuclideanIndividual where
	readsPrec _ = map (first EuclideanIndividual) .  
		(readP_to_S . between (char '(') (char ')') $ sepBy1 (readS_to_P reads) (char ','))
instance Read EuclideanPopulation where
	readsPrec _ = map (first EuclideanPopulation) . (readP_to_S . many1 $
		skipSpaces >> readS_to_P reads)
