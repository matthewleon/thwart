module Data.Dictionary where

import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Prelude

newtype Dictionary = Dictionary (StrMap Unit)
derive newtype instance showDictionary :: Show Dictionary

init :: Dictionary
init = Dictionary $ StrMap.empty

insert :: String -> Dictionary -> Dictionary
insert s (Dictionary d) = Dictionary $ StrMap.insert s unit d

has :: String -> Dictionary -> Boolean
has s (Dictionary d) = StrMap.member s d

size :: Dictionary -> Int
size (Dictionary d) = StrMap.size d
