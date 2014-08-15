{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Serialize.SerializePropUtils where

import Data.ExpObj

getPosStr (NullO p) = posStr p

posStr (Upd (x,y)) = show [x,y]
posStr x           = "SerializeProp::posStr [Unexpected pattern ["++show x++"]]"
