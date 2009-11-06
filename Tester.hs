{-# LANGUAGE NoMonomorphismRestriction,MultiParamTypeClasses,TypeSynonymInstances,FlexibleInstances,FlexibleContexts #-}
import Control.Monad.State
import Control.Arrow
import System.Random
import Control.Applicative
import Data.List
import Debug.Trace

import Evolution
import Genetics
import Children
import Lib.Notes
import Lib.Shuffle

runTest = flip evalState (mkStdGen 0) 

stringPops :: (HasNote [Char] l) => [l]
stringPops = map note ["pippo","io","te","noi","voi","tutti","qualcuno"]

tlog :: LabelledWith LogRate Int cf => [Noted c ls] -> Core cf c [Noted c ls]
tlog = liftM2 (>>) pokeData return 

--tester :: (NoteOf LogRate (l, (LogRate, ()))) =>
--        l
--         -> (Change (Core (l, (LogRate, ())) c) [Noted c ls]) 
--         -> [Noted c ls]
--         -> [c]
tester note a c = runTest $ snd <$> evolve (dumbdrive, (LogRate,1) .< note) (\x -> a x >>= tlog) c

testLog = take 4 $ tester () return (stringPops :: [Noted String ()])

instance Fitted String Double where
	fit = fromIntegral . length

testCulling = take 4 $ tester ((CullPoint,4) .< ()) cullStep (stringPops :: [Noted String ((Fitness,Double),())])


instance Children String String Double where
	inserts x = [x]
	child x y = take 2 x ++ drop 2 y

type InsertsA = ((Parents,ParentsSet),((Fitness,Double),()))
type GeneA = ((Insertions,[Noted String InsertsA]),InsertsA)

testChildrenS = take 20 $ tester ((ParentsPenalty,0.1) .< (Parentado,1) .< (CullChildren,10) .< ()) expand (stringPops :: [Noted String GeneA])

instance Children [Int] [Int] Double where
	inserts x = take 5 $ tails x
	child y x = x ++ (y \\ x)

instance Fitted [Int] Double where
	fit xs = 1 / (fromIntegral . sum . zipWith (\x y -> abs (x - y)) xs $ tail xs)

type GeneIA = ((Insertions,[Noted [Int] InsertsA]),InsertsA)

population = take 10 . map note . runTest . replicateM 10 $ shuffle [1..10::Int] ::[Noted [Int] GeneIA]

tracer xs = trace ("\n" ++ (show $ map ((id &&& fit).datum) xs)) (return xs)  
step x = tracer x >>= expand  >>= cullStep

d = (CullPoint,100) .< (ParentsPenalty,0.0) .< (Parentado,2) .< (CullChildren,100) .< ()

testChildrenI = map fit . take 40 $ tester d step population
