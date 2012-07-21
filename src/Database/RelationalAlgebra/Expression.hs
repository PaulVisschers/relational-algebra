{-# LANGUAGE GADTs #-}
module Database.RelationalAlgebra.Expression where

import Control.Monad.State
import Control.Monad.Reader

import Database.RelationalAlgebra.Record

data Expression s a where
  ConstExpr :: a -> Expression s a
  AttrExpr :: Ref a s -> Expression s a
  UnExpr :: UnaryOperator (a -> b) -> Expression s a -> Expression s b
  BinExpr :: BinaryOperator (a -> b -> c) -> Expression s a -> Expression s b -> Expression s c

data AggregateExpression s b where
  AggrExpr :: AggregateOperator ([a] -> b) -> Expression s a -> AggregateExpression s b

data UnaryOperator a where
  OpNot :: UnaryOperator (Bool -> Bool)
  OpIsNull :: UnaryOperator (Maybe a -> Bool)
  OpIsNotNull :: UnaryOperator (Maybe a -> Bool)
  OpLength :: UnaryOperator (String -> Int)

data BinaryOperator a where
  OpEq :: Eq a => BinaryOperator (a -> a -> Bool)
  OpNotEq :: Eq a => BinaryOperator (a -> a -> Bool)
  OpLt :: Ord a => BinaryOperator (a -> a -> Bool)
  OpLtEq :: Ord a => BinaryOperator (a -> a -> Bool)
  OpGt :: Ord a => BinaryOperator (a -> a -> Bool)
  OpGtEq :: Ord a => BinaryOperator (a -> a -> Bool)
  OpAnd :: BinaryOperator (Bool -> Bool -> Bool)
  OpOr :: BinaryOperator (Bool -> Bool -> Bool)
  OpPlus :: Num a => BinaryOperator (a -> a -> a)
  OpMinus :: Num a => BinaryOperator (a -> a -> a)
  OpMul :: Num a => BinaryOperator (a -> a -> a)
  OpDiv :: Fractional a => BinaryOperator (a -> a -> a)
  OpMod :: Integral a => BinaryOperator (a -> a -> a)
  OpCat :: BinaryOperator (String -> String -> String)
  -- TODO: Add OpLike, OpIn, OpBitNot, OpBitAnd, OpBitOr, OpBitXor and OpAsg.

data AggregateOperator a where
  OpCount :: AggregateOperator ([a] -> Int)
  OpSum :: Num a => AggregateOperator ([a] -> a)
  OpAvg :: Fractional a => AggregateOperator ([a] -> a)
  OpMin :: Ord a => AggregateOperator ([a] -> a)
  OpMax :: Ord a => AggregateOperator ([a] -> a)
  OpFirst :: AggregateOperator ([a] -> a)
  OpLast :: AggregateOperator ([a] -> a)
  -- TODO: Add OpStdDev, OpStdSDevP, OpVar, OpVarP

constant :: a -> Expression s a
constant = ConstExpr

attr :: (Label l a s) => l -> Expression s a
attr l = AttrExpr (ref l)

_not :: Expression s Bool -> Expression s Bool
_not = UnExpr OpNot

_length :: Expression s String -> Expression s Int
_length = UnExpr OpLength

isNull :: Expression s (Maybe a) -> Expression s Bool
isNull = UnExpr OpIsNull

notNull :: Expression s (Maybe a) -> Expression s Bool
notNull = UnExpr OpIsNotNull

(.==.) :: Eq a => Expression s a -> Expression s a -> Expression s Bool
(.==.) = BinExpr OpEq

(.<>.) :: Eq a => Expression s a -> Expression s a -> Expression s Bool
(.<>.) = BinExpr OpNotEq

(.<.) :: Ord a => Expression s a -> Expression s a -> Expression s Bool
(.<.) = BinExpr OpLt

(.<=.) :: Ord a => Expression s a -> Expression s a -> Expression s Bool
(.<=.) = BinExpr OpLtEq

(.>.) :: Ord a => Expression s a -> Expression s a -> Expression s Bool
(.>.) = BinExpr OpGt

(.>=.) :: Ord a => Expression s a -> Expression s a -> Expression s Bool
(.>=.) = BinExpr OpGtEq

(.&&.) :: Expression s Bool -> Expression s Bool -> Expression s Bool
(.&&.) = BinExpr OpAnd

(.||.) :: Expression s Bool -> Expression s Bool -> Expression s Bool
(.||.) = BinExpr OpOr

(.+.) :: Num a => Expression s a -> Expression s a -> Expression s a
(.+.) = BinExpr OpPlus

(.-.) :: Num a => Expression s a -> Expression s a -> Expression s a
(.-.) = BinExpr OpMinus

(.*.) :: Num a => Expression s a -> Expression s a -> Expression s a
(.*.) = BinExpr OpMul

(./.) :: Fractional a => Expression s a -> Expression s a -> Expression s a
(./.) = BinExpr OpDiv

(.%.) :: Integral a => Expression s a -> Expression s a -> Expression s a
(.%.) = BinExpr OpMod

(.++.) :: Expression s String -> Expression s String -> Expression s String
(.++.) = BinExpr OpCat

count :: Expression s a -> AggregateExpression s Int
count = AggrExpr OpCount

_sum :: Num a => Expression s a -> AggregateExpression s a
_sum = AggrExpr OpSum

avg :: Fractional a => Expression s a -> AggregateExpression s a
avg = AggrExpr OpAvg

_min :: Ord a => Expression s a -> AggregateExpression s a
_min = AggrExpr OpMin

_max :: Ord a => Expression s a -> AggregateExpression s a
_max = AggrExpr OpMax