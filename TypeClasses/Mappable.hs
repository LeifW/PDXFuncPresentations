-- {-# OPTIONS_GHC -F -pgmF she #-}
module Mappable where
import Prelude hiding (map)
import Control.Applicative hiding (pure)

-- mapM / traverse in Haskell stblib
--mapA :: Applicative k => (a -> k b) -> [a] -> k [b]
--mapA f = foldr (\x xs -> (| f x : xs |) ) (pure [])

class Wrapper w where
  wrap :: a -> w a

instance Wrapper [] where
  wrap a = [a]

instance Wrapper Maybe where
  wrap a = Just a


-- Mappable
class Mappable w where
  map :: (a -> b) -> w a -> w b

instance Mappable [] where
  map f [] = []
  map f (h:t) = f h : map f t

instance Mappable Maybe where
  map f Nothing = Nothing
  map f (Just x) = Just (f x)


class (Wrapper w, Mappable w) => ApplyableMappable w where
  apply :: w (a -> b) -> w a -> w b

instance ApplyableMappable Maybe where
  apply (Just f) (Just x) = Just (f x)
  apply _ _ = Nothing

-- (+) `map` Just 1 `apply` Just 2
  
(<@>) :: ApplyableMappable w => w (a -> b) -> w a -> w b
(<@>) = apply

-- Coalescable
class Coalescable w where
  coalesce :: w ( w a ) -> w a  -- aka flatten / join

instance Coalescable [] where
  coalesce [] = []
  coalesce (h:t) = h ++ coalesce t

instance Coalescable Maybe where
  coalesce Nothing = Nothing 
  coalesce (Just x) = x




-- Must obey left identity, right identity, and associativity.
--class Functor k => Monad k where
