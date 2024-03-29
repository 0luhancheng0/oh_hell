import Test.QuickCheck
import Test.QuickCheck.Test(isSuccess)
import qualified Test.QuickCheck.Property as P (Result, succeeded, failed, reason)
import OhTypes
import OhHell (bidSize, hookRule, reneging)
import safe Player
import Control.Monad
import System.Exit

instance Arbitrary Card where
  arbitrary = Card <$> arbitraryBoundedEnum <*> arbitraryBoundedEnum

playerCount :: Int
playerCount = 4

players :: [String]
players = ["a", "b", "c", "d"]

-- | Check if player renegs: does not follow led suit if possible
correct :: Card -> [Card] -> [Card] -> P.Result
correct card trick hand
  | reneging card hand ledSuit trick  = P.failed {
      P.reason = unlines [
          "Did not follow suit: [" ++ (show ledSuit) ++ "] " ++ (show card),
          "Hand: " ++ (show hand),
          "Trick: " ++ (show (reverse trick))]}
  | otherwise = P.succeeded
  where
      (Card ledSuit _) = last trick


-- makeBids
makeBids :: Int -> Gen [Int]
makeBids size = do
  p <- choose (0, playerCount - 1)
  replicateM p (choose (1, size))

-- Test Bid function, it should:
--  1. Bid between 0 and the number of cards.
--  2. Respect the hook rule.
test_bidding :: Card -> Gen P.Result
test_bidding trump = do
  size <- choose (3, 12)
  hand <- vector size
  bids <- makeBids size
  nb <- return $ makeBid trump hand playerCount bids
  return $ result hand nb bids
  where
    result hand nb bids
      | not (bidSize nb (length hand)) = P.failed {
          P.reason = unlines [
              "Bid not valid: " ++ (show nb),
              "Hand: " ++ (show hand)] }
      | not (hookRule bids 4 (length hand)) = P.failed {
          P.reason = unlines [
              "Did not respect hook rule",
              "Hand: " ++ (show hand),
              "Bids: " ++ (show bids)] }
      | otherwise = P.succeeded

test_one_play :: Card -> Gen P.Result
test_one_play trump = do
  size <- choose (3, 12)
  h <- choose (1, size)
  hand <- vector h
  bids <- replicateM 3 (choose (1, size))
  nb <- return $ makeBid trump hand playerCount bids
  previous <- replicateM (size - h) (replicateM 4 arbitrary)
  p <- choose (0, playerCount - 1)
  trick <- replicateM p arbitrary
  play <- return $ (
    playCard "a"
    hand
    (zip players (nb: bids))
    trump
    (map (\cs -> zip cs players) previous)
    (zip trick players))
  return $ correct play trick hand

main :: IO ()
main = do
  -- add test runners into the array for each module
  done <- mapM quickCheckResult [test_bidding, test_one_play]
  if all isSuccess done
     then exitSuccess
     else exitFailure
