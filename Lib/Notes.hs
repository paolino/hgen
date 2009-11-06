{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FlexibleContexts,TypeSynonymInstances,FunctionalDependencies , UndecidableInstances,OverlappingInstances,TypeOperators,ViewPatterns #-}
module Lib.Notes where

import Control.Arrow hiding ((+++))
import Control.Applicative hiding (many)
import Text.ParserCombinators.ReadP hiding (look)
import Data.Char (isAlphaNum)
import Control.Parallel.Strategies
class NoteOf l ls where
	see :: ls -> l
	set :: l -> ls -> ls

instance NoteOf l (l,ls) where 
	see (l,_) = l 
	set l (_,ls) = (l,ls) 
instance NoteOf l ls => NoteOf l (l',ls) where 
	see (_,ls) = see ls
	set l (l',ls) = (l',set l ls)

instance NoteOf l ls => NoteOf l (Noted c ls)  where
	see = see . notes
	set x y = set x <$> y

seeset :: NoteOf a ls => (a -> a) -> ls -> ls	
seeset f x =  set  (f $ see x) x

class Label l v r | l r -> v where
	look :: l -> r -> v
	change :: (l,v) -> r -> r

instance  Label l v ((l,v),r) where
	look l ((_,v),_) = v
	change (l,v) (_,r) = ((l,v),r)

instance Label l v r => Label l v ((l',v'),r) where
	look l (_,r) = look l r
	change lv (lv',r) = (lv',change lv r)

instance Label l v r => Label l v (Noted c r)  where
	look l = look l . notes
	change x y = change x <$> y

lookchange l f x  = change (l,f $ look l x) x
lookchangeM l f x = do 	v <- f (look l x) 
			return $ change (l,v) x

data Prova1 = Prova1 deriving Show
data Prova2 = Prova2 deriving Show

infixr 8 .<
-- | by hand adding an annotation
(.<) :: l -> ls -> (l,ls)
(.<) = (,) 

infixr 8 .<.
-- | by hand adding an annotation
(.<.) :: l -> (Noted a ls) -> (Noted a (l,ls))
(.<.) x (Noted y ns) = Noted y (x,ns)

infixr .<<.
-- | a fresh note 
(.<<.) :: l -> a -> Noted a (l,())
(.<<.) x y = Noted y (x,()) 

type a :*: b = (a,b)
infixr 0 :*:
------------------------------------------------------------------------------
class HasNote a l where
	note :: a -> l
instance HasNote a () where 
	note x = ()
instance (HasNote a l1,HasNote a l2) => HasNote a (l1,l2) where
	note x = (note x,note x)

data Noted a ls = Noted {
	datum 	:: a , 		-- ^ the payload
	notes 	:: ls 		-- ^ informations for it
	}  
instance (NFData a , NFData ls) => NFData (Noted a ls) where
	rnf (Noted a ls) = rnf a `seq` rnf ls 

instance Functor (Noted a) where
	f `fmap` (Noted a ls) = Noted a (f ls)

instance HasNote a l => HasNote a (Noted a l) where
	note x = Noted x (note x) 

--instance (HasNote c ls,HasNote ls ls') => HasNote c (Noted ls ls') where 
--	note c = Noted ls (note ls) where ls = note c

instance (Show a,Show ls )=> Show  (Noted a ls) where
	show (Noted x y) = show (x,y)

newtype Note l = Note {unNote ::l} deriving Show 
instance Functor Note where
	fmap f (Note l) = Note (f l)

instance (Read l, Read ls) =>  Read (Note (l,ls)) where
	readsPrec _ s =  let 
		valids x = isAlphaNum x || elem x "\"' " 
		rs	= flip readP_to_S s . many $ 
			skipSpaces >> 
			between (char '(') (char ')') 
				(sepBy1 (munch1 valids) (char ',')) 
		build [l,v] s =  "((" ++ l ++ "," ++ v ++ ")," ++ s ++ ")" 
		in case rs of
			[] -> []
			(last -> (ns,_)) -> case reads $ foldr build "()" ns of 
				[] -> []
				ls -> map (first Note) ls
class Eq a => Name a where
	label :: a
