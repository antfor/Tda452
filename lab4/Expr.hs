module Expr where

import Parsing

--Part I
x :: Expr
x = Var 'x'

num :: Double -> Expr
num  = Num

add :: Expr -> Expr -> Expr
add = Binary Add

mul :: Expr -> Expr -> Expr
mul = Binary Mul

cos :: Expr -> Expr
cos = Unary Cos

sin :: Expr -> Expr
sin = Unary Sin
--A-----------------------------
data Expr = Num Double | Var Char | Unary UnOp Expr | Binary BinOp Expr Expr


class Associativ a where
    assoc :: a -> Assoc

data Assoc = Left | Right | None
    deriving Eq

class Associativ a where
    assoc :: a -> Assoc

instance Associativ Expr where
    assos e = case e of
        Num _ -> None
        Var _ -> None
        Unary op _ -> pres op
        Binary op _ -> pres op

-- https://rosettacode.org/wiki/Operator_precedence#Haskell
class Presidens a where
    pres :: a -> Int

instance Presidens Expr where
    pres e = case e of
        Num _ -> 9
        Var _ -> 9
        Unary op _ -> pres op
        Binary op _ -> pres op

data UnOp  = Sin | Cos

instance UOP UnOp where
    showU Sin = preFix "sin" ""  --B
    showU Cos = preFix "cos " "" --B
    evalU Sin = Prelude.sin      --C
    evalU Cos = Prelude.cos      --C

--    parU _ e@(Binary _ _ _) = addPar e --B
--    parU _ e = showExpr e              --B

instance Presidens UnOp where
    pres e = case e of
        _ -> 9

data BinOp = Mul | Add | Pow

instance BOP BinOp where
    showB Add = inFix " + " --B
    showB Mul = inFix " * " --B
    evalB Add = (+)         --C
    evalB Mul = (*)         --C

--    parB Mul e@(Binary Add _ _) = addPar e --B
--    parB _ e = showExpr e                  --B

instance Presidens UnOp where
    pres e = case e of
        Mul -> 7
        Add -> 6

preFix :: String -> String -> String -> String
preFix op s1 s2 = op ++ s1 ++ s2
inFix :: String -> String -> String -> String
inFix op s1 s2 = s1 ++ op ++ s2
postFix :: String -> String -> String -> String
postFix op s1 s2 = s1 ++ s2 ++ op


class BOP a where
    showB :: a -> String -> String -> String
    evalB :: a -> Double -> Double -> Double
    parB :: a -> Expr -> String

class UOP a where
    showU :: a -> String -> String
    evalU :: a -> Double -> Double
    parU :: a -> Expr -> String

--B-----------------------------

showExpr :: Expr -> String
showExpr (Var c) = show c
showExpr (Num n) = show n
showExpr (Unary op e) = showU op (parU op e)
showExpr (Binary op e1 e2) = showB op (parB op e1) (parB op e2)

par op e | pres op > pres e = inFix (showExpr e) "(" ")"
         | otherwise        = showExpr e
--addPar e = inFix (showExpr e) "(" ")"
--C-----------------------------

eval :: Expr -> Double -> Double
eval (Var c) x = x
eval (Num n) _ = n
eval (Unary op e) n = evalU op (eval e n)
eval (Binary op e1 e2) n = evalB op (eval e1 n) (eval e2 n)

--D-----------------------------

readExpr :: String -> Maybe Expr

--E-----------------------------
--F-----------------------------
--G-----------------------------
