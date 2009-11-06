{-# LANGUAGE FlexibleContexts,ExistentialQuantification,MultiParamTypeClasses,ScopedTypeVariables,FlexibleInstances, TypeSynonymInstances, NoMonomorphismRestriction  #-}
import Control.Monad.RWS
import System.Random
import Data.List

import Control.Concurrent.STM
import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Parallel.Strategies

import Lib.Notes
import Lib.Shuffle
import Debug.Trace

-- | Standard population description
type Population c l = [Noted c l]

-- | Logs constructors
data Show c => LogSystem c = GenericLog String | Solution c deriving Show

-- | the monad in which operators ar programmed
type OperationEnv cf c = RWS cf [LogSystem c] StdGen

instance Randomized (OperationEnv cf c)

-- | signature for any operator
type Operation cf c l = [Population c l] -> OperationEnv cf c [Population c l]

-- | the interthread memory
data Playground cf c l = Playground {
	evolution 	:: TChan (Population c l),  -- ^ populations concentrator and dispatcher
	presences 	:: TVar Int,					 -- ^ actual number of populations
	logs			:: TChan (LogSystem c),		-- ^ message channel 
	config		:: TVar cf						-- ^ live configuration
	}

mkPlayGround :: [Population c l] -> cf -> STM (Playground cf c l)
mkPlayGround ps cf = do 
		logs 			<- newTChan 
		evolution 	<- newTChan 
		mapM (writeTChan evolution) ps
		presences 	<- newTVar (length ps)
		config  		<- newTVar cf
		return $ Playground evolution presences logs config

class Operator a cf c l  where
	operation 	:: a -> Operation cf c l 
	
data OperatorConf = OperatorConf {
	delay :: Int,	-- ^ the delay for the thread
	npops :: Int	-- ^ number of wanted populations
	} deriving Read

turn :: (Operator a cf c l, Label a OperatorConf cf) 
	=> Playground cf c l 
	-> a 
	-> StdGen 
	-> STM ()
turn (Playground  tc tn tlog tcf) o g = do
	cf 	<- readTVar tcf
	n 	<- readTVar tn 
	if (npops . look o $ cf) < n then do	
			ps <- replicateM n (readTChan tc)
			let (ps',_,w) = runRWS (operation o $ ps) cf g
			mapM (writeTChan tc) ps'
			modifyTVar ((+) $  length ps' - length ps) tn
			mapM_ (writeTChan tlog) w
		else return () 	

modifyTVar f x = readTVar x >>= writeTVar x . f 

data BoxOp cf c l = forall a .(Operator a cf c l,Label a OperatorConf cf)  => BoxOp a 

runops :: forall cf c l. 
	[BoxOp cf c l] 	-- ^ operators to run
	-> [Population c l] 			-- ^ populations to evolve
	-> cf 									-- ^ a sensible configuration			
	-> IO (TChan (LogSystem c), TVar cf, IO ()) 	-- ^ the channel for solution output along a thread killer

runops os ps cf = do
	pg <- atomically $ mkPlayGround ps cf
	let 	launch (BoxOp a :: BoxOp cf c l) = do
			delay . look a  <$> atomically (readTVar $ config pg) >>= threadDelay . (*1000)
			newStdGen >>= atomically . turn pg a
	ids <- forM os $ forkIO . forever . launch 
	return (logs pg, config pg, mapM_ killThread ids)
---------------------------------------------------------------------------------------------

-- | an operator to force the evaluation of the populations , use it if your l is too lazy
data Strictman = Strictman deriving Read 
instance (NFData l, NFData c) =>  Operator Strictman cf c l where
	operation Strictman ps =  seq (parMap rnf id ps) $ do
		tell [GenericLog "Forcing population evaluation"]
		return ps

data Mixer = Mixer deriving Read
instance Eq l => Operator Mixer cf c l where
	operation Mixer ps = let
		halve x = splitAt (length x `div` 2) x
		(xss,yss) = unzip . map halve $ ps
		in do
		tell [GenericLog "Mixing populations"] 
		mapM (\xs -> (++) xs <$> pickonelinear yss) xss

data SolutionPicker = SolutionPicker deriving Read
instance Show c => Operator SolutionPicker cf c l where
	operation SolutionPicker ps = do
		pickonelinear (concat ps) >>= tell . return . Solution . datum 
		return ps

-----------------------------------------------------------------------
-------------------- Example  ---------------------------------------
conf =  (Mixer,OperatorConf 5100 3)  
	.< (SolutionPicker, OperatorConf 1000 1) 
	.< (Strictman, OperatorConf 3500 1) .< ()

ops = [BoxOp SolutionPicker,BoxOp Strictman,BoxOp Mixer]	
		 
type Apop = Population Int ()

instance Eq (Noted Int ())

onepop x = map (\x -> Noted x ()) [x, x + 1 .. x + 9] :: [Noted Int ()]
main = do
	 (c,kt,tcf) <- runops ops [onepop 1,onepop 21,onepop 31,onepop 41,onepop 51] conf
	 forever $ atomically (readTChan c) >>= print
