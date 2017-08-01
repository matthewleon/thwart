module Data.Dictionary where

import Data.Foldable (class Foldable)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
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

load :: forall f. Functor f => Foldable f => f String -> Dictionary
load = Dictionary <<< StrMap.fromFoldable <<< map \str -> Tuple str unit

toArray :: Dictionary -> Array String
toArray (Dictionary d) = StrMap.keys d
