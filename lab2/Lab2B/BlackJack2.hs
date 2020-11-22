module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random


hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)

-- A0 ------------------------
{-
Evaluate size hand2 on paper:
size hand2
    = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty));
    = 1 +  size (Add (Card Jack Spades) Empty);
    = 1 + 1 + size Empty;
    = 1 + 1 + 0;
    = 2;
-}

-- A1 ------------------------
display :: Hand -> String
display Empty = ""
display (Add (Card (Numeric r) s) h) =  "\n" ++show r ++ " of "++ show s ++ ", " ++ display h
display (Add (Card r s) h) =  "\n" ++ show r ++ " of "++ show s ++ ", " ++ display h

-- A2 ------------------------
value :: Hand -> Integer
value h
    | sum1 <= 21 = sum1
    | otherwise =  sum1 - (numberOfAces h * 10)
    where sum1 = initialValue h

valueRank :: Rank -> Integer
valueRank (Numeric i) = i
valueRank Ace = 11
valueRank _ = 10

initialValue :: Hand -> Integer
initialValue Empty = 0
initialValue (Add (Card r _) h) = valueRank r + initialValue h

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add (Card _ _) h) = numberOfAces h

-- A3 ------------------------
gameOver :: Hand -> Bool
gameOver hand = 21 < value hand

-- A4 ------------------------
winner :: Hand -> Hand -> Player
winner h1 h2 | not (gameOver h1) && (gameOver h2 || value h1 > value h2) = Guest
winner h1 h2 = Bank

-- B1 ------------------------
(<+) :: Hand -> Hand -> Hand
(<+) h1 = onTopHelper (reverseHand h1)
    where onTopHelper Empty h = h
          onTopHelper (Add c h) h2 = onTopHelper h (Add c h2)

-- | reverses a hand such that the previous top card is now on bottom
reverseHand :: Hand -> Hand
reverseHand h = reverseHelper h Empty
        where reverseHelper Empty h = h
              reverseHelper (Add c h1) h2 = reverseHelper h1 (Add c h2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

rop_size_onTopOf :: Hand -> Hand -> Bool
rop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)

-- B2 ------------------------
fullDeck :: Hand
fullDeck = convert [Card c s | s <- [Hearts, Diamonds, Clubs, Spades], c <-rank]
    where
       convert = foldr Add Empty
       rank = [Ace]++[Numeric k | k <-  [2..10]]++[Jack, Queen, King]

-- B3 ------------------------
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add c h1) h2 = (h1, Add c h2);

  -- B4 ------------------------
playBank :: Hand -> Hand
playBank h = playBankHelper (h, Empty)
    where playBankHelper (deck, hand) | value hand < 16 = playBankHelper(draw deck hand)
                                      | otherwise = hand

-- B5 ------------------------
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g h = shuffleHelper (randomNum g (size h)) (h,Empty) Empty
    where shuffleHelper _ (Empty, h1) h2 = h1 <+ h2
          shuffleHelper (x,g') (h1,h2) h3 = shuffleHelper (randomNum g' (size h1 - 1)) (removeNthCard h1 x) (h2 <+ h3)

-- | genenrates a random number in range [1, i], returning it and a new generator
randomNum:: StdGen -> Integer -> (Integer, StdGen)
randomNum g i = randomR(1, i) g

-- | removes the n:th card from a hand, returning the updated hand and the card
removeNthCard :: Hand -> Integer -> (Hand,Hand)
removeNthCard hand = removeHelper hand Empty
    where removeHelper Empty _ n | n > 0 = error "removeNthCard: Hand is empty"
          removeHelper (Add c h1) h2 1 = (reverseHand h2 <+ h1, Add c Empty)
          removeHelper (Add c h1) h2 n = removeHelper h1 (Add c h2) (n-1)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)

-- B5 ------------------------
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation
