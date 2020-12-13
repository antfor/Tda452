module Expr where

import           Parsing
import           Data.Maybe
import           Data.Char
import QuickCheck

--Part I
x :: Expr
x = Var 'x'

num :: Double -> Expr
num = Num

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


data Assoc = ALeft | ARight | ANone
    deriving Eq

class Expression a where
    assoc :: a -> Assoc
    pres :: a -> Int

-- https://rosettacode.org/wiki/Operator_precedence#Haskell
instance Expression Expr where
  assoc e = case e of
    Num _         -> ANone
    Var _         -> ANone
    Unary op _    -> assoc op
    Binary op _ _ -> assoc op
  pres e = case e of
    Num _         -> 9
    Var _         -> 9
    Unary op _    -> pres op
    Binary op _ _ -> pres op


data UnOp  = Sin | Cos

instance UOP UnOp where
  showU Sin = preFix "sin" ""  --B
  showU Cos = preFix "cos " "" --B
  evalU Sin = Prelude.sin      --C
  evalU Cos = Prelude.cos      --C

instance Expression UnOp where
  assoc e = case e of
    _ -> ARight
  pres e = case e of
    _ -> 9

data BinOp = Mul | Add | Pow

instance BOP BinOp where
  showB Add = inFix " + " --B
  showB Mul = inFix " * " --B
  evalB Add = (+)         --C
  evalB Mul = (*)         --C

instance Expression BinOp where
  assoc e = case e of
    _ -> ALeft
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

class UOP a where
    showU :: a -> String -> String
    evalU :: a -> Double -> Double

size :: Expr -> Int
size Binary _ e1 e2 = 1 + size e1 + size e2
size Unary _ e = 1 + size e
size _ = 0


--B-----------------------------
instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Var c          ) = show c
showExpr (Num n          ) = show n
showExpr (Unary op e     ) = showU op (par op e)
showExpr (Binary op e1 e2) = showB op (par op e1) (par op e2)

par op e | pres op > pres e = inFix (showExpr e) "(" ")"
         | otherwise        = showExpr e

--C-----------------------------

eval :: Expr -> Double -> Double
eval (Var c          ) x = x
eval (Num n          ) _ = n
eval (Unary op e     ) n = evalU op (eval e n)
eval (Binary op e1 e2) n = evalB op (eval e1 n) (eval e2 n)

--D-----------------------------

readExpr :: String -> Maybe Expr
readExpr s = do
  case parse expr s of
    Nothing -> Nothing
    Just e  -> if (null . snd) e then (Just . fst) e else Nothing

expr, term, factor :: Parser Expr -- todo use assoc and pres
expr = foldl1 add <$> chain term (char '+')
term = foldl1 mul <$> chain factor (char '*')
factor = Expr.sin <$> parsTrig "sin"
      <|> Expr.cos <$> parsTrig "cos"
      <|> Num <$> (readsP :: (Parser Double))
      <|> char '(' *>  expr <*  char ')'
      <|> Var 'x'<$  char 'x'
 where
     parsTrig trig = mapM_ char trig *> factor

--E-----------------------------
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = e == readExpr (show e)

arbExpr :: Int -> Gen Expr
arbExpr = undefined

assocExpr :: Expr -> Expr --todo fix
assocExpr (Binary Add (Binary Add e1 e2) e3) = assocExpr (Binary Add e1 (Binary Add e2 e3))
assocExpr (Binary Mul (Binary Mul e1 e2) e3) = assocExpr (Binary Mul e1 (Binary Mul e2 e3))
assocExpr (Binary op e1 e2) = Binary op (assocExpr e1) (assocExpr e2)
assocExpr (Unary op e) = Unary op (assocExpr e)
assocExpr e = e

instance Arbitrary Expr where
  arbitrary = sized arbExpr
--F-----------------------------

simplify :: Expr -> Expr
simplify = undefined

--G-----------------------------

differentiate :: Expr -> Expr
differentiate = undefined
