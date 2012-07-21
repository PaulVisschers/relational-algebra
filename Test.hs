import Database.RelationalAlgebra.Expression
import Database.RelationalAlgebra.Query
import Database.RelationalAlgebra.Record
import Database.RelationalAlgebra.Interface.Pure

data Name = Name
data Age = Age
data Hobby = Hobby
data Person = Person

testDatabase :: FRecord Table (Cons Person PersonScheme Nil)
testDatabase = FCons testTable FNil

type PersonScheme = Cons Name String (Cons Age Int (Cons Hobby String Nil))

testTable :: Table PersonScheme
testTable = Table [
  Cons "Paul" (Cons 25 (Cons "Haskell!!!" Nil)),
  Cons "Jenny" (Cons 25 (Cons "Balloon animals" Nil)),
  Cons "Oliander" (Cons 34 (Cons "Video games" Nil))
  ]