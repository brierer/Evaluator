{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Data.EvalCmd (
	EvalCmd(..),
	Event(..),
	Hook(..),
	EventType(..),
	index', number', old', now'
	) where

import Data.Aeson
import Data.Typeable
import GHC.Generics

data EvalCmd = EvalCmd { _key :: String , _eq :: String , _event :: [Event]} deriving(Generic, Show)
data Event = Event {_type :: EventType , _pos :: Int , _hook :: Hook} deriving (Generic, Show)

data EventType = CreateRow | CreateCol | RemoveRow | RemoveCol | Change deriving (Generic, Show)
data Hook = Hook {index :: Int , number :: Int, old :: String, now :: String}   deriving (Generic, Show)


index' :: Event -> Int
index' (Event _ _ h) = index h

number' :: Event -> Int
number' (Event _ _ h) = number h

old' :: Event -> String
old' (Event _ _ h) = old h

now' :: Event -> String
now' (Event _ _ h) = now h

instance ToJSON EvalCmd
instance FromJSON EvalCmd

instance ToJSON Event
instance FromJSON Event

instance ToJSON EventType
instance FromJSON EventType

instance ToJSON Hook
instance FromJSON Hook