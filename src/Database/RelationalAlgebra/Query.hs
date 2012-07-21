{-# LANGUAGE GADTs, FlexibleContexts #-}
module Database.RelationalAlgebra.Query where

import Database.RelationalAlgebra.Expression
import Database.RelationalAlgebra.Record

data Query db s where
  BaseTable :: Ref s db -> Query db s
  Select :: Expression s Bool -> Query db s -> Query db s
  Project :: Projection s1 s2 -> Query db s1 -> Query db s2
  Binary :: QueryOperator (s1 -> s2 -> s3) -> Query db s1 -> Query db s2 -> Query db s3
  Group :: Eq (Record s2) => Projection s1 s2 -> AggregateProjection s1 s3 -> Query db s1 -> Query db s3

type Projection s = FRecord (Expression s)
type AggregateProjection s = FRecord (AggregateExpression s)

data QueryOperator a where
  OpProduct :: Append a b c => QueryOperator (a -> b -> c)
  OpUnion :: Eq (Record a) => QueryOperator (a -> a -> a)
  OpIntersect :: Eq (Record a) => QueryOperator (a -> a -> a)
  OpDifference :: Eq (Record a) => QueryOperator (a -> a -> a)
  -- TODO: Add OpDivide

table :: Label l s db => l -> Query db s
table l = BaseTable (ref l)

select :: Expression s Bool -> Query db s -> Query db s
select = Select

project :: Projection s1 s2 -> Query db s1 -> Query db s2
project = Project

group :: Eq (Record s2) => Projection s1 s2 -> AggregateProjection s1 s3 -> Query db s1 -> Query db s3
group = group

product :: (Append a b c) => Query db a -> Query db b -> Query db c
product = Binary OpProduct

union :: Eq (Record a) => Query db a -> Query db a -> Query db a
union = Binary OpUnion

intersect :: Eq (Record a) => Query db a -> Query db a -> Query db a
intersect = Binary OpIntersect

difference :: Eq (Record a) => Query db a -> Query db a -> Query db a
difference = Binary OpDifference