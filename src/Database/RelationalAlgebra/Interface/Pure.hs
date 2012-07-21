{-# LANGUAGE GADTs #-}
module Database.RelationalAlgebra.Interface.Pure where

import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Data.List as L (groupBy, union, intersect, (\\))
import Data.Function (on)


import Database.RelationalAlgebra.Record
import Database.RelationalAlgebra.Expression
import Database.RelationalAlgebra.Query

newtype Table s = Table {unTable :: [Record s]}
type Database = FRecord Table

query :: Query dbs s -> Database dbs -> [Record s]
query = evalQuery

insert :: Ref s dbs -> Record s -> Database dbs -> Database dbs
insert s x db = modifyTableField (++ [x]) s db

insertQuery :: Ref s dbs -> Query dbs s -> Database dbs -> Database dbs
insertQuery s q db = modifyTableField (++ evalQuery q db) s db

update :: Ref s dbs -> Expression s Bool -> Projection s s -> Database dbs -> Database dbs
update s p f db = modifyTableField (mapWhere (evalExpression p) (evalProjection f)) s db

delete :: Ref s dbs -> Expression s Bool -> Database dbs -> Database dbs
delete s p db = modifyTableField (filter (not . evalExpression p)) s db

modifyTableField :: ([Record s] -> [Record s]) -> Ref s dbs -> Database dbs -> Database dbs
modifyTableField f = modifyField (Table . f . unTable)

mapWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhere b f = map (\x -> if b x then f x else x)


evalExpression :: Expression s a -> Record s -> a
evalExpression (ConstExpr x) r = x
evalExpression (AttrExpr ref) r = getField ref r
evalExpression (UnExpr op e) r = evalUnaryOperator op (evalExpression e r)
evalExpression (BinExpr op e1 e2) r = evalBinaryOperator op (evalExpression e1 r) (evalExpression e2 r)

evalUnaryOperator :: UnaryOperator (a -> b) -> a -> b
evalUnaryOperator OpNot = not
evalUnaryOperator OpIsNull = isNothing
evalUnaryOperator OpIsNotNull = isJust
evalUnaryOperator OpLength = length

evalBinaryOperator :: BinaryOperator (a -> b -> c) -> a -> b -> c
evalBinaryOperator OpEq = (==)
evalBinaryOperator OpNotEq = (/=)
evalBinaryOperator OpLt = (<)
evalBinaryOperator OpLtEq = (<=)
evalBinaryOperator OpGt = (>)
evalBinaryOperator OpGtEq = (>=)
evalBinaryOperator OpAnd = (&&)
evalBinaryOperator OpOr = (||)
evalBinaryOperator OpPlus = (+)
evalBinaryOperator OpMinus = (-)
evalBinaryOperator OpMul = (*)
evalBinaryOperator OpDiv = (/)
evalBinaryOperator OpMod = mod
evalBinaryOperator OpCat = (++)

evalAggregateExpression :: AggregateExpression s a -> [Record s] -> a
evalAggregateExpression (AggrExpr op e) rs = evalAggregateOperator op (map (evalExpression e) rs)

evalAggregateOperator :: AggregateOperator ([a] -> b) -> [a] -> b
evalAggregateOperator OpCount = length
evalAggregateOperator OpSum = sum
evalAggregateOperator OpAvg = \xs -> sum xs / fromIntegral (length xs)
evalAggregateOperator OpMin = minimum
evalAggregateOperator OpMax = maximum
evalAggregateOperator OpFirst = head
evalAggregateOperator OpLast = last

evalQuery :: Query db s -> FRecord Table db -> [Record s]
evalQuery (BaseTable ref) db = unTable (getField ref db)
evalQuery (Select e q) db = filter (evalExpression e) (evalQuery q db)
evalQuery (Project p q) db = map (evalProjection p) (evalQuery q db)
evalQuery (Binary op q1 q2) db = evalQueryOperator op (evalQuery q1 db) (evalQuery q2 db)
evalQuery (Group p ap q) db = map (evalAggregateProjection ap) (groupList (evalProjection p) (evalQuery q db))

groupList :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupList f = map (map snd) . L.groupBy ((==) `on` fst) . map (\x -> (f x, x))

-- TODO: Check that union, intersect and difference have the correct semantics.
evalQueryOperator :: QueryOperator (a -> b -> c) -> [Record a] -> [Record b] -> [Record c]
evalQueryOperator OpProduct = \r1 r2 -> concatMap (\r -> map (append r) r2) r1
evalQueryOperator OpUnion = L.union
evalQueryOperator OpIntersect = L.intersect
evalQueryOperator OpDifference = (L.\\)

evalProjection :: Projection r1 r2 -> Record r1 -> Record r2
evalProjection FNil r = Nil
evalProjection (FCons e p) r = Cons (evalExpression e r) (evalProjection p r)

evalAggregateProjection :: AggregateProjection r1 r2 -> [Record r1] -> Record r2
evalAggregateProjection FNil rs = Nil
evalAggregateProjection (FCons e p) rs = Cons (evalAggregateExpression e rs) (evalAggregateProjection p rs)