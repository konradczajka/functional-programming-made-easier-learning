module Ch7a where

import Prelude (Unit, show, discard, (==), ($), (<), (>), (<=), (>=))

import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Effect (Effect)
import Effect.Console (log)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)


data Maybe a = Nothing | Just a

derive instance eqMaybe :: Eq a => Eq (Maybe a)
derive instance ordMaybe :: Ord a => Ord (Maybe a)
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

--instance eqMaybe :: Eq a => Eq (Maybe a) where
--  eq (Just x) (Just y) = x == y
--  eq Nothing Nothing = true
--  eq _ _ = false
--
--instance ordMaybe :: Ord a => Ord (Maybe a) where
--  compare (Just x) (Just y) = compare x y
--  compare Nothing Nothing = EQ
--  compare Nothing _ = LT
--  compare _ Nothing = GT
--
--greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
--greaterThanOrEq x y = cmp == EQ || cmp == GT where cmp = compare x y
--
--infixl 4 greaterThanOrEq as >=
--
--instance showMaybe :: Show a => Show (Maybe a) where
--  show (Just x) = "(Just " <> show x <> ")"
--  show _ = "Nothing"

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log  "------------------"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
  log  "------------------"
  log $ show $ Just "abc"
  log $ show $ (Nothing :: Maybe Unit)
  log  "------------------"
  log $ show $ (Left "abc" :: Either _ Unit)
  log $ show $ (Right (Just 42) :: Either Unit _)
  log $ show $ (Left "abc" :: Either _ Unit) == (Left "abc" :: Either _ Unit)
