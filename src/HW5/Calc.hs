{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


module HW5.Calc(eval, evalStr, Expr, lit, add, mul, withVars, computeMaybe, HasVars, var, compile) where

import HW5.ExprT as ExprT
import HW5.VarExprT as VarExprT
import HW5.Parser
import HW5.StackVM as StackVM
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add leftExp rightExp) = eval leftExp + eval rightExp
eval (ExprT.Mul leftExp rightExp) = eval leftExp * eval rightExp

evalStr :: String -> Maybe Integer
evalStr exprString = eval <$> parseExp  ExprT.Lit ExprT.Add ExprT.Mul exprString

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance Expr Integer where
    lit = id
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit = (>0)
    add left right = left || right
    mul left right = left && right

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add  (MinMax left) (MinMax right) = MinMax (max left right)
    mul (MinMax left) (MinMax right) = MinMax (min left right)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7
    add  (Mod7 left) (Mod7 right) = Mod7 ((left + right) `mod` 7)
    mul (Mod7 left) (Mod7 right) = Mod7 ((left * right) `mod` 7)


instance Expr Program where
    lit value = [PushI value]
    add leftProgram rightProgram = leftProgram ++ rightProgram ++ [StackVM.Add]
    mul leftProgram rightProgram = leftProgram ++ rightProgram ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile strExp = parseExp lit add mul strExp :: Maybe Program

class HasVars a where
    var :: String -> a

instance Expr VarExprT where
    lit = VarExprT.Lit
    add = VarExprT.Add
    mul = VarExprT.Mul

instance HasVars VarExprT where
    var = VarExprT.Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit value varNameToIntegerMap = Just value
    add mapperFuncLeft mapperFuncRight varNameToIntegerMap = (+) <$> mapperFuncLeft varNameToIntegerMap <*> mapperFuncRight varNameToIntegerMap
    mul mapperFuncLeft mapperFuncRight varNameToIntegerMap = (*) <$> mapperFuncLeft varNameToIntegerMap <*> mapperFuncRight varNameToIntegerMap

withVars :: [(String, Integer)]
    -> (M.Map String Integer -> Maybe Integer)
    -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

computeMaybe :: Maybe Integer
computeMaybe = withVars [("x", 6)] $ add (lit 3) (var "x")

main = show $ StackVM.stackVM <$> compile "(3 * -4) + 5"
