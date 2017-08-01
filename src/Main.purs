module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array as Array
import Data.Dictionary (Dictionary)
import Data.Dictionary as Dictionary
import Data.Foldable (any)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(Pattern))
import Data.String as String

import Node.Encoding (Encoding(ASCII))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.ReadLine (READLINE, prompt, setLineHandler, setPrompt,  noCompletion, createConsoleInterface)

import Partial.Unsafe (unsafePartial)

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

potentialStartWords :: Dictionary -> String -> Array String
potentialStartWords allWords endWord' =
  Array.filter (not $ hasCharFrom endWord') wordsOfStartLength
  where
    startWordLength = 4
    wordsOfStartLength = Array.filter isLength $ Dictionary.toArray allWords
      where isLength word = String.length word == startWordLength

    hasCharFrom :: String -> String -> Boolean
    hasCharFrom fromStr testStr =
      any (flip Set.member (Set.fromFoldable $ String.toCharArray fromStr))
          (String.toCharArray testStr)

randomElem :: forall a eff. Array a -> Eff (random :: RANDOM | eff) a
randomElem xs =
  unsafePartial $ fromJust <<< Array.index xs <$> randomInt 0 (Array.length xs)

main :: forall e. Eff (console :: CONSOLE, fs :: FS, exception :: EXCEPTION, random :: RANDOM, readline :: READLINE | e) Unit
main = do
  dict <- dictionaryFromFile "src/dictionary.txt"
  startWord <- randomElem $ potentialStartWords dict endWord
  log startWord
  interface <- createConsoleInterface noCompletion
  setPrompt "> " 2 interface
  setLineHandler interface $ \s -> do
    log $ "You typed: " <> s
    prompt interface
