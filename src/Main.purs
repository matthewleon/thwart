module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Dictionary (Dictionary)
import Data.Dictionary as Dictionary
import Data.Set (Set)
import Data.String as String
import Data.String (Pattern(Pattern))

import Node.Encoding (Encoding(ASCII))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)

data GameState = GameState {
  playedWords :: Set String
, score1 :: Int
, score2 :: Int
, turn :: Player
}

data Player = Player1 | Player2

endWord :: String
endWord = "THWART"

dictionaryFromFile
  :: forall eff. 
     FilePath
  -> Eff (fs :: FS, exception :: EXCEPTION | eff) Dictionary
dictionaryFromFile path =
  Dictionary.load <<< String.split (Pattern "\n") <$> readTextFile ASCII path

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
