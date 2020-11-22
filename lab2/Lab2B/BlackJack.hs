module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random
{- Lab 2B
   Date: 18/11/2020
   Authors:Anton Forsberg and Erik Hermansson
   Lab group: 31
 -}
--------------------------------------------
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty);

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

--Evaluate size hand2 in Haskell:
sizeSteps :: [Integer]
sizeSteps = [size hand2
            ,1 + size (Add (Card Jack Spades) Empty)
            ,1 + 1 + size Empty
            ,1 + 1 + 0
            ,2 ]

-- A1 ------------------------

display :: Hand -> String
display Empty = ""
display (Add (Card rank suit) hand) = "\n" ++ cardName rank suit ++ display hand
    where
        cardName :: Rank -> Suit -> String
        cardName (Numeric num ) suit = show num ++ " of " ++ show suit
        cardName court suit = show court ++ " of " ++ show suit

-- A2 ------------------------
--Option 2
value :: Hand -> Integer
value hand | 21 >= sum11 = sum11
           | otherwise = aceValue hand 1
           where
             sum11 = aceValue hand 11

aceValue :: Hand -> Integer -> Integer
aceValue Empty _ = 0
aceValue (Add (Card rank _ ) hand) aceV = valueRank rank + aceValue hand aceV
    where
      valueRank :: Rank -> Integer
      valueRank (Numeric num)  = num;
      valueRank Ace = aceV;
      valueRank _ =  10 ;

-- A3 ------------------------

gameOver :: Hand -> Bool
gameOver hand = 21 < value hand

-- A4 ------------------------

winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest = Bank
                  | guestValue == bankValue = Bank
                  | (guestValue < bankValue)  && not (gameOver bank) = Bank
                  | otherwise = Guest
              where
                  guestValue = value  guest
                  bankValue = value  bank

-- B1 --------------------

(<+) :: Hand -> Hand -> Hand
(<+) Empty hand = hand
(<+) hand Empty = hand
(<+) (Add card Empty) bot = Add card bot
(<+) (Add card hand) bot = Add card (hand <+ bot)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1 + size h2) == size (h1 <+ h2)

-- B2 --------------------

fullDeck :: Hand
fullDeck = convert [Card c s | s <- [Hearts, Diamonds, Clubs, Spades], c <-rank]
    where
       convert = foldr Add Empty
       rank = [Ace]++[Numeric k | k <-  [2..10]]++[Jack, Queen, King]

-- B3 --------------------

draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add c h1) h2 = (h1, Add c h2);

-- B4 --------------------

playBank :: Hand -> Hand
playBank h = playBankHelper (h, Empty)
    where playBankHelper (deck, hand) | value hand < 16 = playBankHelper(draw deck hand)
                                      | otherwise = hand

-- B5 --------------------


shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g Empty = Empty
shuffleDeck g deck = (Add card Empty) <+ (shuffleDeck g1 smallerDeck)
    where
      (index, g1) = randomR (1, size deck) g
      (smallerDeck, card) = removeNth deck index


removeNth :: Hand -> Integer -> (Hand, Card)
removeNth (Add card hand) 1 = (hand, card)
removeNth (Add card hand) n = ((Add card Empty) <+ keep, del)
    where
     (keep , del) =  removeNth hand (n-1)



prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h)

-- B6 --------------------


implementation = Interface{ iFullDeck = fullDeck
                            , iValue = value
                            , iDisplay = display
                            , iGameOver = gameOver
                            , iWinner = winner
                            , iDraw = draw
                            , iPlayBank = playBank
                            , iShuffle = shuffleDeck
                            }

main :: IO ()
main = runGame implementation
