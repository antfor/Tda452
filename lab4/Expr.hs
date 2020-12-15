module Expr where

import           Parsing
import           Data.Maybe
import           Data.Char
import           Data.List
import           Test.QuickCheck


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
data Expr =  Var Char | Num Double | Unary UnOp Expr | Binary BinOp Expr Expr
    deriving (Eq, Ord)


data Assoc = ALeft | ARight | ANone  -- todo remove ???
    deriving Eq

class Expression a where
    assoc :: a -> Assoc     -- todo remove ???
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
    deriving (Eq, Ord)

instance UOP UnOp where
  showU Sin = preFix "sin " ""  --B
  showU Cos = preFix "cos " "" --B
  evalU Sin = Prelude.sin      --C
  evalU Cos = Prelude.cos      --C
  diffU Sin e = mul (Expr.cos e) (diffExpr e)                  --G
  diffU Cos e = mul (mul (Num (-1)) (Expr.sin e)) (diffExpr e) --G

instance Expression UnOp where
  assoc e = case e of
    _ -> ARight
  pres e = case e of
    _ -> 9

data BinOp = Mul | Add
    deriving (Eq, Ord)

instance BOP BinOp where
  showB Add = inFix " + " --B
  showB Mul = inFix " * " --B
  evalB Add = (+)         --C
  evalB Mul = (*)         --C
  diffB Add e1 e2 = add (diffExpr e1) (diffExpr e2)                   --G
  diffB Mul e1 e2 = add (mul (diffExpr e1) e2) (mul e1 (diffExpr e2)) --G

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
    diffB :: a -> Expr -> Expr ->Expr

class UOP a where
    showU :: a -> String -> String
    evalU :: a -> Double -> Double
    diffU :: a -> Expr -> Expr


size :: Expr -> Int
size (Binary _ e1 e2) = 1 + size e1 + size e2
size (Unary _ e     ) = 1 + size e
size _                = 0

--B-----------------------------
instance Show Expr where
  show = showExpr

showExpr :: Expr -> String
showExpr (Var c          ) = "x"
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

--D----------------------------- Todo

readExpr :: String -> Maybe Expr
readExpr s = do
  case parse expr (filter (/= ' ') s) of
    Nothing -> Nothing
    Just e  -> if (null . snd) e then (Just . fst) e else Nothing

expr, term, factor :: Parser Expr -- todo use assoc and pres
expr = foldr1 add <$> chain term (char '+')
term = foldr1 mul <$> chain factor (char '*')
factor =
  Expr.sin
    <$> parsTrig "sin"
    <|> Expr.cos
    <$> parsTrig "cos"
    <|> Num
    <$> (readsP :: (Parser Double))
    <|> char '('
    *>  expr
    <*  char ')'
    <|> Var 'x'
    <$  char 'x'
  where parsTrig trig = mapM_ char trig *> factor

--E-----------------------------
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e =
  assocExpr e == showread e && (showread . showread) e == showread e
  where showread e = fromJust (readExpr (show e))

arbExpr :: Int -> Gen Expr --todo
arbExpr = rExpr


rExpr :: Int -> Gen Expr --todo use size ?
rExpr s = frequency [(1, rNum), (1, return (Var 'x')), (s, rBin s)]
 where
  rNum = elements $ map Num [-5 .. 5]
  rBin s = do
    let s' = s `div` 2
    fun <- elements [1, 2]
    case fun of
      1 -> do
        op <- elements [Sin, Cos]
        e  <- rExpr s'
        return $ Unary op e
      2 -> do
        op <- elements [Add, Mul]
        e1 <- rExpr s'
        e2 <- rExpr s'
        return $ Binary op e1 e2

instance Arbitrary Expr where
  arbitrary = sized arbExpr


assocExpr :: Expr -> Expr --todo fix
assocExpr (Binary Add (Binary Add e1 e2) e3) = assocExpr (add e1 (add e2 e3))
assocExpr (Binary Mul (Binary Mul e1 e2) e3) = assocExpr (mul e1 (mul e2 e3))
assocExpr (Binary op e1 e2) = Binary op (assocExpr e1) (assocExpr e2)
assocExpr (Unary op e) = Unary op (assocExpr e)
assocExpr e = e
--F-----------------------------

simplify :: Expr -> Expr --todo
simplify = assocExpr
 where
  simplify' :: Expr -> Expr
  simplify' (Unary op e     ) = unaryS op (simplify' e)
  simplify' (Binary op e1 e2) = binaryS op (simplify' e1) (simplify' e2)
  simplify' e                 = e

  simplifyBin :: Expr -> Expr
  simplifyBin (Unary op e     ) = unaryS op (simplifyBin e)
  simplifyBin e@(Binary op e1 e2) = expr
      where
          binList = binToList e op
          simList = map simplifyBin binList
          list = h simList (length simList)
          expr    = foldr1 (Binary op) list

          h :: [Expr] -> Int -> [Expr]
          h xs 0 = xs
          h [x] _ = [x]
          h (e1:e2:es) n | s /= s2 = h (s:es) (n-1)
                         | otherwise = h (e2:h(e1:es) (n-1)) (n-1)
                where
                    s = binaryS op e1 e2
                    s2 = Binary op e1 e2

  simplifyBin e                 = e

  binary Add = add
  binary Mul = mul

  moveVar :: Expr -> Expr
  moveVar (Unary op e     ) = Unary op (moveVar e)
  moveVar (Binary op e1 e2) = expr
   where
    binList = binToList (Binary op (moveVar e1) (moveVar e2)) op
    varList = filter containVar binList
    numList = filter (not . containVar) binList
    expr    = foldr1 (Binary op) (varList ++ numList)
  moveVar e = e

  isUnary :: Expr -> Bool
  isUnary  (Unary _ _) = True
  isUnary  (Binary _ e1 e2) = isUnary e1 || isUnary e2
  isUnary  e = False

  containVar :: Expr -> Bool
  containVar (Var _         ) = True
  containVar (Num _         ) = False
  containVar (Unary _ e     ) = containVar e
  containVar (Binary _ e1 e2) = containVar e1 || containVar e2

  -- "smart" constructors
  unaryS :: UnOp -> Expr -> Expr
  unaryS op (Num n) = Num $ evalU op n
  unaryS op e       = Unary op e

  binaryS :: BinOp -> Expr -> Expr -> Expr
  binaryS Add = addS
  binaryS Mul = mulS

  addS :: Expr -> Expr -> Expr
  addS (Num 0) e                             = e
  addS e       (Num 0)                       = e
  addS (Num n) (Num m)                       = Num (n + m)

  addS e1 e2 | e1 == e2                      = mul (Num 2) e1      -- e+e = e * 2
  addS e1 (Binary Mul (Num n) e2) | e1 == e2 = mul (Num (n + 1)) e1--e + n*e = (n+1)*e
  addS (Binary Mul (Num n) e1) e2 | e1 == e2 = mul (Num (n + 1)) e1-- n*e + e = (n+1) *e
  addS (Binary Mul e1 (Num m)) (Binary Mul e2 (Num n)) | e1 == e2 = mul (Num (n + m)) e1 --e*m + e*n = e*(n+m)

  addS e1 e2 = add e1 e2

  mulS :: Expr -> Expr -> Expr
  mulS (Num 1) e       = e
  mulS e       (Num 1) = e
  mulS (Num 0) e       = Num 0
  mulS e       (Num 0) = Num 0
  mulS (Num n) (Num m) = Num (n * m)
  mulS e1      e2      = mul e1 e2

binToList :: Expr -> BinOp -> [Expr]
binToList (Binary op1 e1 (Binary op2 e2 e3)) op0 | all (op0 ==) [op1, op2] = [e1, e2] ++ binToList e3 op0
binToList (Binary op1 e1 e2) op0 | op0 == op1 = [e1, e2]
binToList e _ = [e]

prop_Simplify :: Expr -> Double -> Bool
prop_Simplify e n = maxSimplified && simplified
 where
  maxSimplified = simplify (simplify e) == simplify e
  simplified    = num (eval e n) == simplify (replaceVar e)
  replaceVar :: Expr -> Expr
  replaceVar (Var _          ) = num n
  replaceVar (Unary op e     ) = Unary op (replaceVar e)
  replaceVar (Binary op e1 e2) = Binary op (replaceVar e1) (replaceVar e2)
  replaceVar e                 = e

--G-----------------------------

differentiate :: Expr -> Expr
differentiate = simplify . diffExpr . simplify


diffExpr :: Expr -> Expr
diffExpr (Var _          ) = Num 1
diffExpr (Num _          ) = Num 0
diffExpr (Unary op e     ) = diffU op e
diffExpr (Binary op e1 e2) = diffB op e1 e2
