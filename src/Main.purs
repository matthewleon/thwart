module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.ST (runST, newSTRef, readSTRef, writeSTRef)
import Data.Array as Array
import Data.Dictionary (Dictionary)
import Data.Dictionary as Dictionary
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(Pattern), toCharArray)
import Data.String as String
import Node.Encoding (Encoding(ASCII))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.ReadLine (READLINE, prompt, setLineHandler, setPrompt, noCompletion, createConsoleInterface)
import Partial.Unsafe (unsafePartial)

newtype GameState = GameState {
  playedWords :: Set String
, previousWord :: String
, score1 :: Int
, score2 :: Int
, currentPlayer :: Player
}
derive instance genericGameState :: Generic GameState _
instance showGameState :: Show GameState where
  show = genericShow

initGameState :: String -> GameState
initGameState startWord = GameState {
    playedWords: Set.empty
  , previousWord: startWord
  , score1: 0
  , score2: 0
  , currentPlayer: Player1
  }

data Player = Player1 | Player2
derive instance eqPlayer :: Eq Player
instance showPlayer :: Show Player where
  show Player1 = "Player1"
  show Player2 = "Player2"

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

  let initialGameState = initGameState startWord

  interface <- createConsoleInterface noCompletion
  setPrompt "> " 2 interface

  runST do
    currentState <- newSTRef initialGameState
    setLineHandler interface $ \s -> do
      currentState' <- readSTRef currentState
      log $ show currentState'
      case turn currentState' dict s of
        Right newState -> void $ writeSTRef currentState newState
        otherwise -> log "word not in dictionary"
      prompt interface

-- TODO: monadify
data TurnError = TurnWordNotInDictionary | TurnWordUnplayable
turn :: GameState -> Dictionary -> String -> Either TurnError GameState
turn (GameState gs) dict str
  | dict `Dictionary.has` str =
    let playedWords' = Set.insert gs.previousWord gs.playedWords
    in  Right <<< GameState $ case gs.currentPlayer of
          Player1 -> {
            playedWords: playedWords'
          , previousWord: str
          , score1: gs.score1 + movePoints str gs.previousWord gs.playedWords
          , score2: gs.score2
          , currentPlayer: Player2
          }
          Player2 -> {
            playedWords: playedWords'
          , previousWord: str
          , score1: gs.score1
          , score2: gs.score2 + movePoints str gs.previousWord gs.playedWords
          , currentPlayer: Player1
          }
  | otherwise = Left TurnWordNotInDictionary


data Play = AddedLetters (NonEmpty Array Char)
          | RemovedLetters (NonEmpty Array Char)
          | Pass -- TODO: should this be legal?

play :: String -> String -> Maybe Play
play oldWord newWord =
  case compare (String.length oldWord) (String.length newWord) of
    EQ -> if oldWord == newWord then Just Pass else Nothing
    LT -> AddedLetters <$> play' oldWord newWord
    GT -> RemovedLetters <$> play' newWord oldWord
  where
  play' :: String -> String -> Maybe (NonEmpty Array Char)
  play' shortWord longWord =
    (Array.uncons $ go (toCharArray shortWord) (toCharArray longWord))
      <#> \{head: c, tail: cs'} -> c :| cs'
    where
    go :: Array Char -> Array Char -> Array Char
    go scs lcs = case Array.uncons scs of
      Just {head: c, tail: cs} -> case Array.elemIndex c lcs of
        Just i  -> go cs $ Array.drop (i + 1) lcs
        Nothing -> lcs
      Nothing -> lcs


-- TODO: should be able to just count array size from playableWord
movePoints :: String -> String -> Set String -> Int
movePoints newWord lastWord playedWords
  | Set.member newWord playedWords = 0
  | otherwise = abs $ String.length newWord - String.length lastWord

