{-# LANGUAGE ScopedTypeVariables #-}
-- module ParallelEvolution (evolve, Response (..)) where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Applicative
import System.Random
import Control.Monad
import Debug.Trace
import System.IO.Unsafe

getTChanContents :: TChan a -> IO [a]
getTChanContents ch = unsafeInterleaveIO $ do
        x  <- atomically $ readTChan ch
        xs <- getTChanContents ch
        return (x:xs)
    
peek n c = replicateM n $ readTChan c
modifyTVar f t = readTVar t >>= writeTVar t . f

atomicallyR m = ReaderT $ atomically . runReaderT m
atomicallyRl = atomicallyR . lift
forkIOR m = ReaderT $ forkIO . runReaderT m

------------------------------------------------------------------

data Response c a = ModifyConf (c -> c) | NewStuff [a] | Undo
type Operation c a w = [a] -> RWS c [w] StdGen (Response c a)

data Operator c a w = Operator {
	operation 	:: Operation c a w, 	-- ^ an operator 
	wantedObjects 	:: Int,			-- ^ number of objects necessary to run
	desiredUse	:: Float		-- ^ percentage of the shoots
	}

data CountingOperator c a w = CountingOperator {
	operator 	:: Operator c a w,
	useCount 	:: Int			-- ^ number of shoots till now
	}

data Playground c a w = Playground {
	logs 		:: TChan w,
	evolution	:: TChan a,
	conf 		:: TVar c,
	opcount 	:: TVar Int
	}

playground conf xs = do
		ws 	<- newTChan 
		as 	<- newTChan
		cf 	<- newTVar conf
		opcount <- newTVar 0
		forM_ xs $ writeTChan as
		return $ Playground ws as cf opcount

checkStatistics :: CountingOperator c a w -> ReaderT (Playground c a w) STM ()
checkStatistics  (CountingOperator (Operator _ _  d) uc) = do
	n <- opcount <$> ask >>= lift . readTVar 
	lift $ check (fromIntegral uc / fromIntegral (n + 1) < d) -- wait for statistics to permit

 
eval :: (Operation c a w , [a]) -> ReaderT (Playground c a w) IO ()
eval (o,xs) = do 
	g <- lift $ newStdGen
	Playground l e c _ <-  ask
	c' <- atomicallyRl $ readTVar c
	let (y,_,ws) = runRWS (o xs) c' g 
	case y of	NewStuff ys  	-> mapM_ (atomicallyRl . writeTChan e) ys
			Undo 		-> mapM_ (atomicallyRl . unGetTChan e) xs
			ModifyConf f 	-> atomicallyRl $ modifyTVar f c	
	mapM_ (atomicallyRl . writeTChan l) ws
		

runOp r@(CountingOperator (Operator o wo _) uc) = do
	Playground l e _ n 	<- ask
	atomicallyR $ (lift (isEmptyTChan l >>= check) >> checkStatistics r) 	-- stop on too many shots and log no read
	xs 			<- atomicallyRl $ peek wo e -- get the wanted number of objects
	forkIOR . eval $ (o,xs) 		-- run the operator in a separate thread
	atomicallyRl $ modifyTVar (+1) n	-- update overall counter
	runOp r{useCount = uc + 1}		-- rerun setting the operator use count	

evolve	:: forall c a w . [Operator c a w] 	-- ^ the operators to run along their attributes
	-> c 		-- ^ a starting configuration	
	-> [a] 		-- ^ the stuff to operate on
	-> [w]		-- ^ the logs channel 
evolve rops c xs = unsafePerformIO $ do
	p <- atomically (playground c xs) 
	runReaderT (forM_ rops' $ forkIOR . runOp) p 
	getTChanContents $ logs p
	where rops' = map (\o -> CountingOperator o 0) rops

{-------------------------------------------------------------------------------------------------------
checkResponse :: Bool -> ([a] -> Response c a) 
checkResponse t x = if t then NewStuff x else Undo
r1,r2,r3	::Operator () [Int] [Int] 
r1 = Operator (\[x] -> return $ checkResponse (x < 50) [x+17]) 	1 0.4 
r2 = Operator (\[x] -> return $ checkResponse (x > 40) [x `div` 2]) 	1 0.5 
r3 = Operator (\[x,y] -> if abs (x-y) < 10 then tell [x,y] >> return (NewStuff []) else return Undo) 	2 0.1 
main = print (evolve [r1,r2,r3] () [1..5000] !! 4999)
-}
