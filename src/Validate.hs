module Validate
  ( Validator(..)
  , Validated(..)
  , label
  , validate
  , fromPredicate
  , minLength
  , maxLength
  , ordMin
  , ordMax
  )
where

import           Data.Functor.Contravariant

data Validated e = Valid | Invalid e
  deriving (Show, Eq)

newtype Validator a = Validator { runValidator :: a -> Validated String }

-- like maybe
instance Functor Validated where
  fmap f Valid       = Valid
  fmap f (Invalid e) = Invalid (f e)

-- both must be valid, otherwise keep error
instance Semigroup (Validated e) where
  (Invalid e) <> _  = Invalid e
  Valid       <> v2 = v2

-- allows to reuse validators by transformations, e.g.
--    myValidate = contramap show (maxLength 5)
--    validate myValidate 123456
instance Contravariant Validator where
  contramap f v = Validator $ runValidator v . f

-- both Validate must accept a
instance Semigroup (Validator a) where
  v1 <> v2 = Validator $ runValidator v1 <> runValidator v2

instance Monoid (Validator a) where
  mempty = Validator $ const Valid

-- usefull helper which performs a `Validate a` over an `a`
-- and returns the value if valid or the error message
validate :: Validator a -> a -> Either String a
validate iv val = case runValidator iv val of
  Valid         -> Right val
  Invalid error -> Left error

-- overrides error message of a validator
label :: Validator a -> String -> Validator a
label iv msg = Validator $ \a -> fmap (const msg) (runValidator iv a)

fromPredicate :: (a -> Bool) -> Validator a
fromPredicate predicate =
  Validator $ \a -> if predicate a then Valid else Invalid "validate.predicate"

maxLength :: Foldable t => Int -> Validator (t a)
maxLength boundary =
  fromPredicate (\t -> length t <= boundary) `label` "validate.length.max"

minLength :: Foldable t => Int -> Validator (t a)
minLength boundary =
  fromPredicate (\t -> length t >= boundary) `label` "validate.length.min"

ordMax :: Ord t => t -> Validator t
ordMax m = fromPredicate (<= m) `label` "validate.max"

ordMin :: Ord t => t -> Validator t
ordMin m = fromPredicate (>= m) `label` "validate.min"
