{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib.Counter where

import System.IO.Unsafe
import Data.Unique

counter _ = hashUnique $ unsafePerformIO $ newUnique
