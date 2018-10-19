module Player (
    playCard,
    makeBid
)
where
{-
Before the start of each hand, the make bid function will return a integer
according to multiple criteria of the cards in hand
1. the number of ace and king in card, each of them contribute 1 to bid
2. the number of ace in all non-trump cards, contributed bid will be the total
  number divide by two (integer division)
3. if the the cards in hand in average greater than (1 - 1/(number of player)) of all possible cards
  and the sum of others bid is less than the expected sum of bid, then another bid will be added
The strategy will tend to underbid rather than overbid

For playCard function, it has been divided into three cases
For the case that current score is less than the bid
  consider how many large cards are in hand, if too many, then throw the card while keep bid under control
    if there isn't a card that can be throwed while keep lose trick, then throw the largest one to attemp win the trick
  if too few large cards, then if there is no card can win, then throw the middle card, which will make following trick easier
  else return the larest card in suit to attemp win
For the case that current score is equal to the bid
  always play the largest card that guarantee lose, if there is no such card guarantee lose, then play the smallest
For the case that current score is larger than bid
  the trick is screwed, then throw largest or smallest card to mess up others

-}

import OhTypes
import OhHell
import Data.List
import Data.Maybe

-- a hand of cards
data Cards = Cards {
  renegCards::[Card],
  trumpCards::[Card],
  leadCards::Maybe [Card]
}

playCard :: PlayFunc
playCard myid cs bids trump trickPlayed currentTrick = case compare currentScore myBid of
  LT ->
    if length largeCards > (myBid - currentScore) then -- if there is too many large cards
      case loseTrick of -- consider the cards guarantee lose
        [] -> cardsMinimum myCards -- play minimum card to lose current trick
        _ -> if null (intersect largeCards loseTrick) then
            if null largeCards then maximum loseTrick else last largeCards
          else
            last (intersect largeCards loseTrick) -- play the largest card that can lose the current trick
    else
      if elem (cardsMaximum myCards) loseTrick then
        takeMid myCards -- any playable card will lose the trick, take a middle one, to make the following easier
      else
        cardsMaximum myCards -- try win the current trick

  -- if the current score is equal to the bid number
  EQ ->
    case loseTrick of -- consider those cards guarantee lose
      [] -> cardsMinimum myCards -- play minimum card in cards
      _ -> last loseTrick -- play the largest card of them

  -- if the current score is already over the target bid, then play cards
  -- that may break others bid
  GT ->
    if null largeCards then
      cardsMinimum myCards
    else
      last largeCards
  where
    -- get the lead card wrapped in maybe
    maybeLeadCard = (pure currentTrick >>= (\x -> case x of
      [] -> Nothing
      (_:_) -> Just $ fst $ last currentTrick))
    playerNum = length bids -- number of players
    myCards = sortCards trump maybeLeadCard cs -- construct the cards in hand
    myBid = snd $ head $ filter (\(a, _) -> a == myid) bids
    currentScore = foldl (\current -> \pid -> if pid == myid then current + 1 else current) 0 ((winner $ cardSuit trump) <$> trickPlayed)
    currentPossibleCards = sortedDeck \\ (foldl (\acc -> \(c, _) -> c:acc) [] (concat trickPlayed)) -- get the difference between all cards with the cards have been played

    --  get the palyabel cards that has probility that greater than (1 - 1/number of player)
    largeCards = getWinProbCards (1 - 1/(fromIntegral playerNum))
                                (case leadCards myCards of
                                  Nothing -> renegCards myCards ++ trumpCards myCards
                                  Just lc -> lc)
                                currentPossibleCards
    loseTrick = guaranteeLoseTrick trump myCards currentTrick -- get the cards that guarantee lose in current trick

-- return the minimum card in a Cards
cardsMinimum :: Cards -> Card
cardsMinimum cs = case leadCards cs of
  Nothing -> minimum (renegCards cs ++ trumpCards cs)
  Just ls -> minimum ls

-- return the maximum card in Cards
cardsMaximum :: Cards -> Card
cardsMaximum cs = case leadCards cs of
  Nothing -> maximum (renegCards cs ++ trumpCards cs)
  Just ls -> maximum ls

-- return the proper bid number
makeBid :: BidFunc
makeBid trump cs playerNum bids =
  let
    myCards = sortCards trump Nothing cs -- construct the cards in hand
    avgWinExpectation = averageWinExpectation trump (renegCards myCards ++ trumpCards myCards) sortedDeck -- the expected win probility
    forbidenBid = length cs - sum bids
    standardBS = standardBidSum playerNum cs
    bidSum = fromIntegral $ sum bids :: Double
    targetBid = (numOfAce (trumpCards myCards))
      + (numOfKing (trumpCards myCards))
      + quot (numOfAce (renegCards myCards)) 2
      + if (avgWinExpectation >  (1 - 1/(fromIntegral playerNum)) && bidSum < standardBS) then 1 else 0
  in
    if targetBid /= forbidenBid then
      targetBid
    else
      -- if target bid is equal to the forbidenBid, then take one less, or if the forbidenBid is 0, then take 1
      if targetBid == 0 then
        1
      else
        targetBid - 1

-- | return the set of card that would guarantee the lose of current trick
-- it is done by get all the cards that less that the max cards that has been played
guaranteeLoseTrick :: Card -> Cards -> Trick -> [Card]
guaranteeLoseTrick _ _ [] = [] -- if the player is the first in current trick, there is no way to guarantee lose
guaranteeLoseTrick trump myCards trick =
  case (leadCards myCards) of
    Just ls -> -- if the player can follow the lead suit
      if null trumpCardsPlayed then ls else -- check if trump has been played
        if suitEq trump (head ls) then filter ((<) (maximum trumpCardsPlayed)) ls else ls -- check the lead suit is trump suit
    Nothing -> -- if the player cannot follow the lead suit
      if not (null trumpCardsPlayed) then -- if trump has been played, the play the reneg cards or trump cards that less the maximum trump played
        renegCards myCards ++
        filter ((<) (maximum (cardsOfSuitInTrick (cardSuit trump) trick))) (trumpCards myCards)
      else -- if trump is not played, then paly reneg card to ensure lose
        renegCards myCards
  where
    trumpCardsPlayed = trumpPlayed trump trick -- all the trumps that has been played in the current trick

-- | get the cards that above one perticular number (within [0, 1]) in a specified domain
getWinProbCards :: Double -> [Card] -> [Card] -> [Card]
getWinProbCards winProb cs domain = fst <$> (filter (\(c, d) -> (elem c cs) && (d > winProb)) (cardsWinExpectation domain))

-- | all the trump card has been played in a given trick
trumpPlayed :: Card -> Trick -> [Card]
trumpPlayed trump trick = fst <$> filter (\(c, _) -> suitEq trump c) trick

-- | number of ace and king in all cards
aceKingNum :: Double
aceKingNum = 8.0

-- | total number of cards
totalCardsNum :: Double
totalCardsNum = 52.0

-- | filter out the cards have the given suit
cardsOfSuitInTrick :: Suit -> Trick -> [Card]
cardsOfSuitInTrick suit trick = fmap fst (filter (\(c, _) -> cardSuit c == suit) trick)

-- | return the expected total bid number if other player consider number of king and ace as the bid number
standardBidSum :: Int -> [Card] -> Double
standardBidSum playerNum cs = (aceKingNum/totalCardsNum) * fromIntegral (playerNum * (length cs))

-- | for each card in a sorted card list calculate the chance of win under that cards
cardsWinExpectation :: [Card] -> [(Card, Double)]
cardsWinExpectation cs =  zip cs ((\x -> (fromIntegral x) / fromIntegral (length cs)) <$> [1 .. (length cs)])

-- | produce a double value represent how likely a hand of cards would win
-- in the domain of all cards
averageWinExpectation :: Card -> [Card] -> [Card] -> Double
averageWinExpectation trump cs cardsDomain = (foldl (\acc -> \(_, p) -> acc + p) 0 l) / (fromIntegral (length l))
  where
    sortedCardsDomain = sortCards trump Nothing cardsDomain
    sortedCardsDomainList = (renegCards sortedCardsDomain) ++ (trumpCards sortedCardsDomain) -- sort all cards
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
          ((Just $ sort [x | x <- myCards, islead x]) >>= (\x -> if null x then Nothing else Just x))
    where
      istrump = suitEq trump
      islead c = case leadCard of
        Nothing -> False
        Just lc -> suitEq lc c


-- | suitEq test if given two card have the same suit
suitEq :: Card -> Card -> Bool
suitEq a b = cardSuit a == cardSuit b

-- | cardSuit return the suit of a given card
cardSuit :: Card -> Suit
cardSuit (Card s _) = s

-- | cardRank return the rank of a given card
cardRank :: Card -> Rank
cardRank (Card _ r) = r

-- | take the playable card that is in the middle of hand
takeMid :: Cards -> Card
takeMid myCards =
  case leadCards myCards of
    Nothing -> rtCards !! (quot (length rtCards) 2)
    Just lc -> lc !! (quot (length lc) 2)
  where
    rtCards = (renegCards myCards ++ trumpCards myCards)
