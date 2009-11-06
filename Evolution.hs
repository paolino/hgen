{-# LANGUAGE FunctionalDependencies,UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeSynonymInstances,ViewPatterns,ScopedTypeVariables #-}

module Evolution (
	Driver (..),
	evolve,
	dumbdrive,
	Core,
	EvolutionLog (..),
	ConfigFile (..)
	)
	where

import Control.Monad.RWS (RWS,runRWS,lift,put,get,local,ask,tell)
import Control.Monad.State (runState,StateT,State,execStateT)
import System.Random (StdGen)
import Control.Applicative ((<$>))
import Control.Exception

import Lib.Shuffle (Randomized)

import Lib.Notes
import System.IO.Unsafe

import Control.Parallel.Strategies


-- | A driver for the algorithm should be changing the configuration checking the Population each turn
-- and having a personal state
class Driver b c cf | b -> c, b -> cf where
		modulate :: [c] -> State b (Either EvolutionError (cf -> cf))
		modulate = const $ return (Right id)

data EvolutionError = DriverError String | CoreError String deriving Show
data Show c => EvolutionLog c =  LogError EvolutionError | LogSolution c deriving Show

-- | The don't care driver is done passing undefined :: () as initial state to drivers
instance Driver () c cf 
dumbdrive = undefined :: ()

type Change m a = a -> m a

type Core cf c = RWS cf [EvolutionLog c] StdGen 

instance Randomized (Core cf l)

type Evolution b cf l = StateT b (Core cf l)

-- | Wraps a mutation operator in a complete never ending evolution driver controlled
evolution :: (Driver b c cf , NFData c) => Change (Core cf l) [c] -> Change (Evolution b cf l) [c]
evolution step cs 	=  rnf cs `seq` 
			do	l <- runState (modulate cs) <$> get
				mf <- case l of 
					(Left e,_) -> tell [LogError e] >> return id
					(Right mf,b) -> put b >> return mf
				local mf (lift (step cs) >>= evolution step)

-- | Calculate the stream of genomes with their fitness  from the monad runner and the driver state and some genomes
evolve :: (Driver b c cf, Randomized m,NFData c)
	=> (b,cf)			-- ^ initial state for the driver and the initial config
	-> Change (Core cf l) [c]  	-- ^ step to iterate
	-> [c]				-- ^ starting populations
	-> m (b,[EvolutionLog l])			-- ^ the driver final state and the output, under the Randomized monad
evolve (dc,tc) step cs =  do
	(dc',g',cs') <- runRWS (execStateT (evolution step cs) dc) tc <$> get 
	put g'
	return (dc',cs')
----------------------------------------------------------------------------------------------------


------------------------------------------------------
--
data ConfigFile = ConfigFile {filename :: String}

instance Read (Note cf) => Driver ConfigFile c cf where
	modulate _ = do 
		s <- get
		case unsafePerformIO . try . readFile . filename $ s of
			Left (e :: IOException) -> return $ Left (DriverError $ "IO Error " ++ show e)
			Right r -> return $ case reads r of
				[] -> Left (DriverError "Couldn't parse config file")
				(last -> (unNote -> cf,_)) -> Right (const cf)
