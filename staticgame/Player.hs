module Player (
    playCard,
    makeBid
)
where
{-
Write a report describing your design and strategy here.
-}

import OhTypes
import OhHell
import Data.List
import Data.Maybe

data Cards = Cards {
  renegCards::[Card],
  trumpCards::[Card],
  leadCards::Maybe [Card]
}

playCard :: PlayFunc
playCard myid cs bids trump trickPlayed currentTrick
  | currentScore < myBid =
    if length largeCards > (currentScore - myBid) then
      case loseTrick of
        [] -> cardsMinimum myCards
        _ -> if null (intersect largeCards loseTrick) then
          last largeCards
          else
            last (intersect largeCards loseTrick)
    else
      cardsMaximum myCards
  | currentScore == myBid =
    case loseTrick of
      [] -> cardsMinimum myCards
      _ -> last loseTrick
  | currentScore > myBid =
    if null largeCards then
      cardsMinimum myCards
    else
      case largeCards of
        [] -> case leadCards myCards of
          Nothing -> cardsMinimum myCards
          Just ls -> minimum ls
        _ -> last largeCards
    where
      maybeLeadCard = (pure currentTrick >>= (\x -> case x of
        [] -> Nothing
        (_:_) -> Just $ fst $ last currentTrick))

      myCards = sortCards trump maybeLeadCard cs
      myBid = snd $ head $ filter (\(a, _) -> a == myid) bids
      currentScore = foldl (\current -> \pid -> if pid == myid then current + 1 else current) 0 ((winner $ cardSuit trump) <$> trickPlayed)
      currentPossibleCards = sortedDeck \\ (foldl (\acc -> \(c, _) -> c:acc) [] (concat trickPlayed))
      largeCards = getWinProbCards 0.8 (renegCards myCards ++ trumpCards myCards) currentPossibleCards
      loseTrick = guaranteeLoseTrick trump myCards currentTrick


cardsMinimum :: Cards -> Card
cardsMinimum cs = case leadCards cs of
  Nothing -> minimum (renegCards cs ++ trumpCards cs)
  Just ls -> minimum ls

cardsMaximum :: Cards -> Card
cardsMaximum cs = case leadCards cs of
  Nothing -> maximum (renegCards cs ++ trumpCards cs)
  Just ls -> maximum ls

    -- currentLeadSuit >>= (\x -> case x of
    --   Nothing -> Just $ chooseLeadCard  )




-- chooseLeadCard :: [Card] -> Card

-- | Bid the number of cards you can win based on the trump card and your hand.
--   last player to bid must obey "hook rule":
--   sum of bids must not equal number of tricks available
-- BidFunc = Card    -- ^ trump card
-- -> [Card] -- ^ list of cards in the player's hand
-- -> Int    -- ^ number of players
-- -> [Int]  -- ^ bids so far
-- -> Int
-- BidFunc :: Card -> [Card] -> Int -> [Int] -> Int

makeBid :: BidFunc
makeBid trump cs playerNum bids =
  let
    myCards = sortCards trump Nothing cs
    avgWinExpectation = averageWinExpectation trump (renegCards myCards ++ trumpCards myCards) sortedDeck
    forbidenBid = length cs - sum bids
    standardBS = standardBidSum playerNum cs
    bidSum = fromIntegral $ sum bids :: Double
    targetBid = (numOfAce (trumpCards myCards ++ renegCards myCards)) + quot (numOfKing (trumpCards myCards)) 2
      + if (avgWinExpectation > 0.8 && bidSum < standardBS) then 1 else 0
  in
    if targetBid /= forbidenBid then
      targetBid
    else
      if targetBid == 0 then
        1
      else
        targetBid - 1

-- | return the set of card that would guarantee the lose of current trick
-- it is done by get all the cards that less that the max cards that has been played
guaranteeLoseTrick :: Card -> Cards -> Trick -> [Card]
guaranteeLoseTrick _ _ [] = []
guaranteeLoseTrick trump myCards trick =
  case (leadCards myCards) of
    Just ls ->
      if (trumpPlayed trump trick) then
        ls
      else
        filter ((<) $ (maximum (cardsOfSuitInTrick (cardSuit $ head ls) trick))) ls
    Nothing ->
      if (trumpPlayed trump trick) then
        renegCards myCards ++
        filter ((<) $ (maximum (cardsOfSuitInTrick (cardSuit trump) trick))) (trumpCards myCards)
      else
        renegCards myCards

-- | found this function not useful at end, keep as backup
-- leadSuit :: Trick -> Maybe Suit
-- leadSuit myCards
--   | null myCards = Nothing
--   | otherwise = let (Card suit _, _) = last myCards in Just suit



-- | domain must be sorted in ascending order when passed in this function
-- A backup interface for Cards, but didn't found it very useful
-- highWinProb :: Double -> Cards -> [Card] -> Trick -> [Card]
-- highWinProb winProb myCards domain [] =
--   case trumpCards myCards of
--     [] -> getWinProbCards winProb (renegCards myCards) domain
--     _ -> getWinProbCards winProb (renegCards myCards ++ trumpCards myCards) domain
-- highWinProb winProb myCards domain _ =
--   case (leadCards myCards) of
--     Just ls -> getWinProbCards winProb ls domain
--     Nothing -> getWinProbCards winProb (trumpCards myCards) domain


getWinProbCards :: Double -> [Card] -> [Card] -> [Card]
getWinProbCards winProb cs domain = map fst (filter (\(c, d) -> (elem c cs) && (d > winProb)) (cardsWinExpectation domain))

trumpPlayed :: Card -> Trick -> Bool
trumpPlayed trump trick = any (\(c, _) -> suitEq trump c) trick


-- | filter out the cards have the given suit
cardsOfSuitInTrick :: Suit -> Trick -> [Card]
cardsOfSuitInTrick suit trick = fmap fst (filter (\(c, _) -> cardSuit c == suit) trick)


standardBidSum :: Int -> [Card] -> Double
standardBidSum playerNum cs = (1/13) * fromIntegral (playerNum * (length cs))

-- | for each card in a sorted card list calculate the chance of win under that cards
cardsWinExpectation :: [Card] -> [(Card, Double)]
cardsWinExpectation cs =  zip cs ((\x -> (fromIntegral x) / fromIntegral (length cs)) <$> [1 .. (length cs)])


-- | produce a double value represent how likely a hand of cards would win
-- in the domain of all cards
averageWinExpectation :: Card -> [Card] -> [Card] -> Double
averageWinExpectation trump cs cardsDomain = (foldl (\acc -> \(_, p) -> acc + p) 0 l) / (fromIntegral (length l))
  where
    sortedCardsDomain = sortCards trump Nothing cardsDomain
    sortedCardsDomainList = (renegCards sortedCardsDomain) ++ (trumpCards sortedCardsDomain)
    l = filter (\(c, _) -> elem c cs) (cardsWinExpectation sortedCardsDomainList)








-- processCards :: ([Card], [Card]) -> ((Double, Double), (Int, Int), (Int, Int))
-- processCards (a, b) = ((mean a, mean b), (median a, median b), (diviation a, diviation b))
numOfAce :: [Card] -> Int
numOfAce = length . (filter ((==Ace) . cardRank))
numOfKing :: [Card] -> Int
numOfKing = length . (filter ((==King) . cardRank))
-- | sortCards take a trump card, the lead suit and the a list of
-- cards then produce a list of sorted cards accordingly
sortCards :: Card -> Maybe Card -> [Card] -> Cards
sortCards trump leadCard myCards
  | leadCard == Nothing =
    Cards (sort [x | x <- myCards, not (istrump x) && not (islead x)])
          (sort [x | x <- myCards, (istrump x) && not (islead x)])
          Nothing
  | otherwise =
    Cards (sort [x | x <- myCards, not (istrump x) && not (islead x)])
          (sort [x | x <- myCards, (istrump x) && not (islead x)])
          (Just $ sort [x | x <- myCards, islead x])
    where
      istrump = suitEq trump
      islead c
        | leadCard == Nothing = True
        | otherwise = suitEq (fromJust leadCard) c


-- | suitEq test if given two card have the same suit
suitEq :: Card -> Card -> Bool
suitEq a b = cardSuit a == cardSuit b

-- | cardSuit return the suit of a given card
cardSuit :: Card -> Suit
cardSuit (Card s _) = s

-- | cardRank return the rank of a given card
cardRank :: Card -> Rank
cardRank (Card _ r) = r
