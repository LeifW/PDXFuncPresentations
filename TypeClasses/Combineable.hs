module Combineable where
import Prelude hiding ((+), (*), sum, (||))
import GHC.Num (plusInteger, timesInteger)

infixl 7 *
infixl 6 +

-- + must be associative
-- (a + b) + c == a + (b + c)
class Combineable a where
  (+) :: a -> a -> a

instance Combineable Integer where
  (+) = plusInteger

instance Combineable [a] where
  a + b = a ++ b

instance Combineable a => Combineable (Maybe a) where
  Just a + Just b = Just (a + b)
  x + Nothing = x
  Nothing + x = x

  
-- must be the identity element for that operation
-- meaning: x + identity must == x, for all x in the set
class Combineable a => ComebineableWithBlank a where
  blank :: a

sum :: ComebineableWithBlank a => [a] -> a
sum = foldr (+) blank
  
instance ComebineableWithBlank Integer where
  blank = 0

instance ComebineableWithBlank [a] where
  blank = []

instance Combineable a => ComebineableWithBlank (Maybe a) where
  blank = Nothing

-- inverse x + x should == identity, for all x in the set
class ComebineableWithBlank a => ComebineableWithBlankAndInverse a where
  inverse :: a -> a

instance ComebineableWithBlankAndInverse Integer where
  inverse x = -x

-- Must obey distributive law: a * ( b + c ) == a * b + a * c
class ComebineableWithBlankAndInverse a => ComebineableWithBlankAndInverseAndMult a where
  (*) :: a -> a -> a

instance ComebineableWithBlankAndInverseAndMult Integer where
  (*) = timesInteger

--class Or a where
--  orElse :: a -> a -> a

class (ComebineableWithBlank a, Eq a) => Or a where
  orElse :: a -> a -> a
  orElse a b = if a == blank then a else b

(||) :: Or a => a -> a -> a
(||) = orElse
--instance ComebineableWithBlank a => Or a where
--  orElse a b = undefined -- if a == blank then a else b
