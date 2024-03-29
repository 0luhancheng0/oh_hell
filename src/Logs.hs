{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Trustworthy #-}
module Logs(
  writeHand,
  clearLog
  )
where

import Data.Time.Clock.System (systemSeconds, getSystemTime)
import Data.Csv
import Data.List (sort, sortBy, elemIndex)
import OhTypes
import System.FilePath
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

instance ToField HandScore where
  toField = toField . score

instance ToField Suit where
  toField = toField . toChar

logDir :: FilePath
logDir = "logs/"

logHeader :: [(String, String, String, String, String, String, String)]
logHeader = [("time", "pos", "bid", "score", "first", "trump", "tricks")]

logFile :: PlayerId -> IO FilePath
logFile playerId = do
  createDirectoryIfMissing False logDir
  return $ logDir ++ playerName ++ ".csv"
  where
    playerName = takeBaseName playerId

fromCards :: [Card] -> String
fromCards (cs: cards) = foldl (\str c -> str ++ "," ++ toId c) (toId cs) cards
  where
    toId :: Card -> String
    toId (Card suit rank) = toChar suit ++ r
      where
        r | rank < Jack = show (fromEnum rank + 2)
          | otherwise = take 1 (show rank)

toChar :: Suit -> String
toChar suit = case suit of
  Spade -> "S"
  Club -> "C"
  Diamond -> "D"
  Heart -> "H"

toCards :: [Trick] -> String
toCards (tr: tricks) = foldl
  (\ts t -> ts ++ ";" ++ getCards t)
  (getCards tr)
  tricks
  where
    getCards = fromCards . (map fst)

fromScores :: PlayerId -> [HandScore] -> Int
fromScores pid scores = (score . head) $
  filter (\HandScore{playerId} -> playerId == pid) scores

clearLog :: Player -> IO ()
clearLog Player{playerId=p} = logFile p >>= \f -> BL.writeFile f (encode logHeader)

writeHand :: [Player] -> HandResult -> IO ()
writeHand players (HandResult (Card suit _) tricks scores) = do
  stamp <- getSystemTime
  let row player result = [
        (systemSeconds stamp,
          elemIndex player pids,
          bid (result :: HandScore),
          fromScores player scores,
          first,
          suit,
          toCards ordered)]
  mapM_ (\(p, q) -> logFile p >>= \f ->
            BL.appendFile f (encode (row p q))) (zip pids points)
  where
    -- Extract player ids and sort them
    pids =  (sort . map (\Player{playerId} -> playerId)) players
    points = sorted (\HandScore{playerId=p} HandScore{playerId=q} -> compare p q) scores
    ordered = (reverse . (map match)) tricks
    match = sortBy (\x y -> compare (snd x) (snd y))
    sorted field = sortBy field
    first = elemIndex (((\Player{playerId} -> playerId) . head) players) pids
