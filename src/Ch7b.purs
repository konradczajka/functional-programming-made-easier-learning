module Ch7b where

import Prelude
import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Int (fromString)

newtype CSV = CSV String

derive instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV
class FromCSV a where
  fromCSV :: CSV -> Maybe a

newtype FullName = FullName String
instance showFullName :: Show FullName where
  show (FullName name) = name
derive instance eqFullName :: Eq FullName

newtype Age = Age Int
derive newtype instance showAge :: Show Age
derive instance eqAge :: Eq Age

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow
derive instance eqOccupation :: Eq Occupation

data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }
derive instance genericPerson :: Generic Person _
instance showPerson :: Show Person where
  show = genericShow
derive instance eqPerson :: Eq Person

instance toCSVPerson :: ToCSV Person where
  toCSV (Person {name, age, occupation}) = CSV $ show name <> "," <> show age <>"," <> show occupation

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

instance fromCSVPerson :: FromCSV Person where
    fromCSV (CSV str) = case split (Pattern ",") str of
        [name, age, occupation] -> case fromString age of
            Just age' -> case toOccupation occupation of
                Just occupation' -> Just $ Person
                    { name: FullName name
                    , age: Age age'
                    , occupation: occupation'
                    }
                Nothing -> Nothing
            Nothing -> Nothing
        _ -> Nothing

test :: Effect Unit
test = do
    log $ show $ toCSV
        (Person
        { name: FullName "Sue Smith"
        , age: Age 23
        , occupation: Doctor
        }) == CSV "Sue Smith,23,Doctor"
    let person = Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            }
    log $ show $ (toCSV person # fromCSV) == Just person
    log $ show $ fromCSV
        (CSV "Sue Smith,23,Doctor")
        == Just (Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            })