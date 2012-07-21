{-# LANGUAGE GADTs, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances, RankNTypes, OverlappingInstances #-}
module Database.RelationalAlgebra.Record where

data Nil
data Cons l a r

data Record r where
  Nil :: Record Nil
  Cons :: a -> Record r -> Record (Cons l a r)

cons :: l -> a -> Record r -> Record (Cons l a r)
cons _ = Cons

instance Eq (Record Nil) where
  _ == _ = True

instance (Eq a, Eq (Record r)) => Eq (Record (Cons l a r)) where
  (Cons x r1) == (Cons y r2) = (x == y) && (r1 == r2)

instance Show (Record Nil) where
  showsPrec n Nil = showString "Nil"

instance (Show a, Show (Record s)) => Show (Record (Cons l a s)) where
  showsPrec n (Cons x xs) = showParen (n > 10) $ showString "Cons " . showsPrec 11 x . showString " " . showsPrec 11 xs

data Ref a r where
  Zero :: Ref a (Cons l a r)
  Suc :: Ref a r -> Ref a (Cons l b r)

class Label l a r | l r -> a where
  ref :: l -> Ref a r

instance Label l a (Cons l a r) where
  ref l = Zero

instance (Label l1 a r) => Label l1 a (Cons l2 b r) where
  ref l = Suc (ref l)

class Field r a b | r a -> b where
  getField :: Ref a s -> r s -> b
  putField :: Ref a s -> b -> r s -> r s

modifyField :: Field r a b => (b -> b) -> Ref a s -> r s -> r s
modifyField f s r = putField s (f (getField s r)) r

instance Field Record a a where
  getField Zero (Cons x _) = x
  getField (Suc s) (Cons _ r) = getField s r

  putField Zero x (Cons _ r) = Cons x r
  putField (Suc s) x (Cons y r) = Cons y (putField s x r)

class Append r1 r2 r3 | r1 r2 -> r3 where
  append :: Record r1 -> Record r2 -> Record r3

instance Append Nil r2 r2 where
  append _ r2 = r2

instance Append r1 r2 r3 => Append (Cons l a r1) r2 (Cons l a r3) where
  append (Cons x r1) r2 = Cons x (append r1 r2)

data FRecord f r where
  FNil :: FRecord f Nil
  FCons :: f a -> FRecord f r -> FRecord f (Cons l a r)

fcons :: l -> f a -> FRecord f r -> FRecord f (Cons l a r)
fcons _ = FCons

toFRecord :: (forall a. a -> f a) -> Record r -> FRecord f r
toFRecord f Nil = FNil
toFRecord f (Cons x r) = FCons (f x) (toFRecord f r)

instance Field (FRecord f) a (f a) where
  getField Zero (FCons x _) = x
  getField (Suc s) (FCons _ r) = getField s r

  putField Zero x (FCons _ r) = FCons x r
  putField (Suc s) x (FCons y r) = FCons y (putField s x r)