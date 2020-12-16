module Expr1 where

import Parsing
import Data.Char
import Data.Maybe
import Test.QuickCheck

-- A ------------------------
data Expr = Num Double | Opr Opr Expr Expr | Func Func Expr | Var -- | Sin Expr | Cos Expr | X-- | Funcs -- | Var x
    deriving (Show, Eq)
--data Nums = Fractional Double -- | Num Integer
--data X =
data Opr = Add | Mul
    deriving (Show, Eq)
--data Ops = Add | Mul
data Func = Sin | Cos
    deriving (Show, Eq)

x :: Expr
x = Var
num :: Double -> Expr
num d = (Num d)
add,mul :: Expr -> Expr -> Expr
add e1 e2 = Opr Add e1 e2
mul e1 e2 = Opr Mul e1 e2
sin',cos' :: Expr -> Expr
sin' e = Func Sin e
cos' e = Func Cos e

size :: Expr -> Int
size (Num d) = 1
size (Var) = 1
size (Opr op e1 e2) = 1 + size e1 + size e2
size (Func func e) = 1 + size e
--size _ _ _ = 3

-- B ------------------------
showExpr :: Expr -> String
showExpr (Num d) = show d
showExpr (Opr op e1 e2) = "("++showExpr e1 ++showOpr op++showExpr e2++")"
showExpr (Func func e) = "("++showFunc func ++ showExpr e++")"
showExpr Var =  "x"

showFunc Cos = "cos "
showFunc Sin = "sin "
showOpr Add = "+"
showOpr Mul = "*"

-- C ------------------------
eval :: Expr -> Double -> Double
eval Var x = x
eval (Num d) x = d
eval (Opr op e1 e2) x =  evalOpr op (eval e1 x) (eval e2 x)
eval (Func func e) x = evalFunc func (eval e x)

evalOpr Add = (+)
evalOpr Mul = (*)
evalFunc Sin = sin
evalFunc Cos = cos

-- D ------------------------

readExpr :: String -> Maybe Expr
readExpr s = let s' = filter (not.isSpace) s
             in case parse expr s' of
                Just (e,"") -> Just e
                _           -> Nothing


expr   = foldl1 (Opr Add) <$> chain term (char '+')
term   = foldl1 (Opr Mul) <$> chain factor (char '*')
sinFactor = (Func Sin) <$> ((stringParser "sin") *> expr)
cosFactor = (Func Cos) <$> ((stringParser "cos") *> expr)
var = char 'x' *> return Var
factor = number <|> sinFactor <|> cosFactor <|> var <|> char '(' *> expr <* char ')'


number :: Parser Expr
number =  Num <$> readsP

stringParser :: String -> Parser String
stringParser str = sequence [char s | s <- str]

-- E ------------------------
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = e == (fromJust $ readExpr $ showExpr e)


numGen :: Gen Expr
numGen = do
            d <- (arbitrary::Gen Double)
            return $ Num d

oprGen :: Int -> Gen Expr
oprGen n = do
             opr <- oneof [return Add , return Mul]
             -- -1 for operator, -1 for second expression
             let s1 = n-2
             n1 <- choose (1,s1)
             e1 <- arbExpr n1
             let s2 = (s1 - (size e1)) + 1
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
arbExpr n = do
                 e <- oneof [oprGen n, funcGen n]
                 return e

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- F ------------------------
simplify :: Expr -> Expr
simplify e | e == e' = e
           | otherwise = simplify e'
           where e' = simplify' e
                 simplify' e = case e of
                    (Opr op (Num n1) (Num n2)) -> (Num ((evalOpr op) n1 n2))
                    (Opr Mul _ (Num 0.0)) -> Num 0
                    (Opr Mul (Num 0.0) _) -> Num 0
                    (Opr Mul (Num 1.0) e) -> simplify e
                    (Opr Mul e (Num 1.0)) -> simplify e
                    (Opr op Var e) -> Opr op Var (simplify e)
                    (Opr op e Var) -> Opr op (simplify e)  Var
                    (Func func (Num n))  -> (Num ((evalFunc func) n))
                    (Func func e)  -> (Func func (simplify e))
                    (Opr op e1 e2) -> (Opr op (simplify e1) (simplify e2))
                    _ -> e

--properties for simplify
prop_SimplifyCorrect :: Expr -> Double -> Bool
prop_SimplifyCorrect e d = eval (simplify e) d == eval e d

prop_SimplifyMin :: Expr -> Bool
prop_SimplifyMin e = isNum e' || isVar e'
                    where e' = simplify e
                          isNum (Num d) = True
                          isNum _ = False
                          isVar e = case e of
                                    (Var) -> (True)
                                    (Func f e) -> (isVar e)
                                    (Opr op e1 e2) -> ((isVar e1) || (isVar e2))
                                    _ -> (False)
-- G ------------------------
differentiate :: Expr -> Expr
differentiate e = (\x -> simplify x) (differentiate' e)
        where differentiate' e = case e of
                (Num _) -> (Num 0.0)
                (Var)   -> (Num 1.0)
                (Opr Add e1 e2) -> (Opr Add (differentiate' e1) (differentiate' e2))
                (Opr Mul e1 e2) -> (Opr Add (Opr Mul e1 (differentiate' e2)) (Opr Mul (differentiate' e1) e2))
                (Func Sin e) -> (Func Cos e)
                (Func Cos e) -> (Opr Mul (Num (-1.0)) (Func Sin e))
