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

-- | get the cards that above one perticular number (within [0, 1]) in a specified domain
getWinProbCards :: Double -> [Card] -> [Card] -> [Card]
getWinProbCards winProb cs domain = map fst (filter (\(c, d) -> (elem c cs) && (d > winProb)) (cardsWinExpectation domain))

-- | check if the trump card has been played in a given trick
trumpPlayed :: Card -> Trick -> Bool
trumpPlayed trump trick = any (\(c, _) -> suitEq trump c) trick


-- | filter out the cards have the given suit
cardsOfSuitInTrick :: Suit -> Trick -> [Card]
cardsOfSuitInTrick suit trick = fmap fst (filter (\(c, _) -> cardSuit c == suit) trick)

-- | return the expected total bid number if other player consider number of king and ace as the bid number 
standardBidSum :: Int -> [Card] -> Double
standardBidSum playerNum cs = (8/52) * fromIntegral (playerNum * (length cs))

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

-- | return the number of Ace in a list cards
numOfAce :: [Card] -> Int
numOfAce = length . (filter ((==Ace) . cardRank))

-- | return the number of King in a list of cards
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
