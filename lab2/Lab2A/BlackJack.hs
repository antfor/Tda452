module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
{- Lab 2A
   Date: 10/11/2020
   Authors:Anton Forsberg and Erik Hermansson
   Lab group: 31
 -}

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
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty);

sizeSteps :: [Integer]
sizeSteps = [size hand2
            ,1 + size (Add (Card Jack Spades) Empty)
            ,1 + 1 + size Empty
            ,1 + 1 + 0
            ,2 ]

-- A1 ------------------------

display :: Hand -> String
display Empty = ""
display (Add (Card rank suit) hand) = cardName rank suit ++ "\n" ++ display hand
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
winner h1 h2 | (not (gameOver h1)) && (gameOver h2 || value h1 > value h2) = Guest
winner h1 h2 = Bank
