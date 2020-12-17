module Expr where

import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck


{- Lab 4B (Standard lab)
   Date: 16/12/2020
   Authors: Anton Forsberg and Erik Hermansson
   Lab group: 31
 -}

-- A ------------------------
data Expr = Num Double | Opr Opr Expr Expr | Func Func Expr | Var -- | Sin Expr | Cos Expr | X-- | Funcs -- | Var x
    deriving (Eq)

data Opr = Add | Mul
    deriving (Eq)

data Func = Sin | Cos
    deriving (Eq)

x :: Expr
x = Var

num :: Double -> Expr
num  = Num

add,mul :: Expr -> Expr -> Expr
add = Opr Add
mul = Opr Mul

sin,cos :: Expr -> Expr
sin  = Func Sin
cos  = Func Cos

size :: Expr -> Int
size (Num d) = 1
size Var = 1
size (Opr op e1 e2) = 1 + size e1 + size e2
size (Func func e) = 1 + size e


-- B ------------------------
showExpr :: Expr -> String
showExpr (Num d) = show d
showExpr f@(Opr op e1 e2) = addPar f e1 ++ showOpr op ++ addPar f e2
showExpr f@(Func func e) = showFunc func ++ addPar f e
showExpr Var =  "x"

showFunc :: Func -> String
showFunc Cos = "cos "
showFunc Sin = "sin "

showOpr :: Opr -> String
showOpr Add = "+"
showOpr Mul = "*"

addPar :: Expr -> Expr -> String
addPar f e | precedence f > precedence e = "(" ++ showExpr e ++ ")"
           | otherwise        = showExpr e

-- use the same precedence as haskell (https://rosettacode.org/wiki/Operator_precedence#Haskell)
precedence :: Expr -> Int
precedence  Var = 9
precedence (Num _) = 9
precedence (Func func _) = 9
precedence (Opr op _ _) = presOpr op
    where
        presOpr :: Opr -> Int
        presOpr Add = 6
        presOpr Mul = 7

instance Show Expr where
    show = showExpr
-- C ------------------------
eval :: Expr -> Double -> Double
eval Var x = x
eval (Num d) x = d
eval (Opr op e1 e2) x =  evalOpr op (eval e1 x) (eval e2 x)
eval (Func func e) x = evalFunc func (eval e x)

evalOpr:: Opr -> Double -> Double -> Double
evalOpr Add = (+)
evalOpr Mul = (*)

evalFunc :: Func -> Double -> Double
evalFunc Sin = Prelude.sin
evalFunc Cos = Prelude.cos

-- D ------------------------

readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                Just (e,"") -> Just e
                _           -> Nothing


expr :: Parser Expr
expr   = foldr1 (Opr Add) <$> chain term (char '+')


term :: Parser Expr
term   = foldr1 (Opr Mul) <$> chain factor (char '*')

sinFactor :: Parser Expr
sinFactor = Func Sin <$> (stringParser "sin" *> factor)

cosFactor:: Parser Expr
cosFactor = Func Cos <$> (stringParser "cos" *> factor)

var::Parser Expr
var = char 'x' *> return Var

factor:: Parser Expr
factor = number <|> sinFactor <|> cosFactor <|> var <|> char '(' *> expr <* char ')'


number :: Parser Expr
number =  Num <$> readsP

stringParser :: String -> Parser String
stringParser str = sequence [char s | s <- str]

-- E ------------------------
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = assocExpr e == fromJust (readExpr $ showExpr e)


assocExpr :: Expr -> Expr
assocExpr (Opr Add (Opr Add e1 e2) e3) = assocExpr (add e1 (add e2 e3))
assocExpr (Opr Mul (Opr Mul e1 e2) e3) = assocExpr (mul e1 (mul e2 e3))
assocExpr (Opr op e1 e2) = Opr op (assocExpr e1) (assocExpr e2)
assocExpr (Func op e) = Func op (assocExpr e)
assocExpr e = e

numGen :: Gen Expr
numGen = do Num <$> (arbitrary :: Gen Double)

oprGen :: Int -> Gen Expr
oprGen n = do
             opr <- oneof [return Add , return Mul]
             -- -1 for operator, -1 for second expression
             let s1 = n-2
             n1 <- choose (1,s1)
             e1 <- arbExpr n1
             let s2 = (s1 - size e1) + 1
             --n2 <- choose (1,s2)
             e2 <- arbExpr s2
             return $ Opr opr e1 e2

funcGen :: Int -> Gen Expr
funcGen n = do
                func <- oneof [return Sin, return Cos]
                --n1 <-choose (1,n-1)
                e <- arbExpr (n-1)
                return $ Func func e

arbExpr :: Int -> Gen Expr
arbExpr n | n < 2 = oneof [numGen, return Var]
arbExpr 2 = funcGen 2
arbExpr n = do oneof [oprGen n, funcGen n]


instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- F ------------------------

simplify :: Expr -> Expr
simplify = assocExpr . loop . assocExpr
 where
  loop e | e == e'   = e
         | otherwise = loop e'
    where e' = simplify' $ assocExpr e

  simplify' :: Expr -> Expr
  simplify' (  Func op e     ) = unaryS op (simplify' e)
  simplify' e@(Opr op e1 e2) = expr
   where
    binList = binToList e op
    simList = map simplify' binList
    list    = simplifyBin simList
    expr    = foldr1 (Opr op) list

    simplifyBin :: [Expr] -> [Expr]
    simplifyBin [] = []
    simplifyBin (e : es) | isNothing res = e : simplifyBin es
                         | otherwise     = simplifyBin $ fromJust res
      where res = maybeSimplify e es

    maybeSimplify :: Expr -> [Expr] -> Maybe [Expr]
    maybeSimplify e1 [] = Nothing
    maybeSimplify e1 (e2 : es) | bs /= b       = Just (bs : es)
                    | isNothing res = Nothing
                    | otherwise     = Just (e2 : fromJust res)
     where
      res = maybeSimplify e1 es
      bs  = binaryS op e1 e2
      b   = Opr op e1 e2

  simplify' e = e

  binToList :: Expr -> Opr -> [Expr]
  binToList (Opr op1 e1 (Opr op2 e2 e3)) op0 | all (op0 ==) [op1, op2] =
    [e1, e2] ++ binToList e3 op0
  binToList (Opr op1 e1 e2) op0 | op0 == op1 = [e1, e2]
  binToList e _ = [e]

  binary :: Opr -> (Expr -> Expr -> Expr)
  binary Add = add
  binary Mul = mul

  -- "smart" constructors
  unaryS :: Func -> Expr -> Expr
  unaryS op (Num n) = Num $ evalFunc op n
  unaryS op e       = Func op e

  binaryS :: Opr -> Expr -> Expr -> Expr
  binaryS Add = addS
  binaryS Mul = mulS

  addS :: Expr -> Expr -> Expr
  addS (Num 0) e                             = e
  addS e       (Num 0)                       = e
  addS (Num n) (Num m)                       = Num (n + m)

  addS e1 e2 | e1 == e2                      = mul (Num 2) e1      -- e+e = e * 2
  addS e1 (Opr Mul (Num n) e2) | e1 == e2 = mul (Num (n + 1)) e1--e + n*e = (n+1)*e
  addS (Opr Mul (Num n) e1) e2 | e1 == e2 = mul (Num (n + 1)) e1-- n*e + e = (n+1) *e
  addS (Opr Mul (Num m) e1) (Opr Mul (Num n) e2) | e1 == e2 = mul (Num (n + m)) e1 --e*m + e*n = e*(n+m)

  addS e1 e2 = add e1 e2

  mulS :: Expr -> Expr -> Expr
  mulS (Num 1) e       = e
  mulS e       (Num 1) = e
  mulS (Num 0) e       = Num 0
  mulS e       (Num 0) = Num 0
  mulS (Num n) (Num m) = Num (n * m)
  mulS e1      (Num m) = mul (Num m) e1
  mulS e1      e2      = mul e1 e2

--properties for simplify
prop_SimplifyCorrect :: Expr -> Double -> Bool -- can fail due to floating point precision
prop_SimplifyCorrect e d = abs (eval (simplify e) d-eval e d) < 1e-3

prop_SimplifyMin :: Expr -> Bool
prop_SimplifyMin e = isNum e' || isVar e'
                    where e' = simplify e
                          isNum (Num d) = True
                          isNum _ = False
                          isVar e = case e of
                                    Var -> True
                                    Func f e -> isVar e
                                    Opr op e1 e2 -> isVar e1 || isVar e2
                                    _ -> False
-- G ------------------------

differentiate :: Expr -> Expr
differentiate = simplify . diffExpr . simplify
 where
    diffExpr :: Expr -> Expr
    diffExpr  Var              = Num 1
    diffExpr (Num _          ) = Num 0
    diffExpr (Func op e     ) = Opr Mul (diffExpr e) (diffFunc op e)
    diffExpr (Opr op e1 e2)   = diffOpr op e1 e2

    diffFunc :: Func -> Expr -> Expr
    diffFunc Sin e = Func Cos e
    diffFunc Cos e = Opr Mul (Num (-1.0)) (Func Sin e)

    diffOpr :: Opr -> Expr -> Expr -> Expr
    diffOpr Add e1 e2 = Opr Add (diffExpr e1) (diffExpr e2)
    diffOpr Mul e1 e2 = Opr Add (Opr Mul (diffExpr e1) e2) (Opr Mul e1 (diffExpr e2))
