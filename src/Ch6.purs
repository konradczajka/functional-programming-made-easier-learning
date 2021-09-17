module Ch6 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (sort)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype, unwrap)

class HasName a where
  name :: a -> String

data Person = Person { firstName :: String, lastName :: String}
instance hasNamePerson :: HasName Person where
  name (Person p) = p.firstName <> " " <> p.lastName

data Company = Company String
instance hasNameCompany :: HasName Company where
  name (Company name) = name

data Place = First | Second | Third

--instance showPlace :: Show Place where
--  show First = "1."
--  show Second = "2."
--  show Third = "3."
derive instance genericPlace :: Generic Place _
instance showPlace :: Show Place where
  show = genericShow


instance eqPlace :: Eq Place where
  eq First First = true
  eq Second Second = true
  eq Third Third = true
  eq _ _ = false

instance ordPlace :: Ord Place where
    compare First First = EQ
    compare First _ = LT
    compare Second Third = LT
    compare Second Second = EQ
    compare Second First = GT
    compare Third Third = EQ
    compare Third _ = GT

x :: Array Place
x = [Third, First, Second]
sx :: Array Place
sx = sort x

data SomeType = This | That | TheOther | AndYetAnother
derive instance eqSomeType :: Eq SomeType
derive instance ordSomeType :: Ord SomeType
derive instance genericSomeType :: Generic SomeType _
instance showSomeType :: Show SomeType where
  show = genericShow

data Address = Address
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

class HasAddress a where
  getAddress :: a -> Address

newtype Person2 = Person2
  { name :: String
  , age :: Int
  , address :: Address
  }

instance hasAddressPerson :: HasAddress Person2 where
  getAddress (Person2 p) = p.address

--newtype Ceo = Ceo Person2
--derive instance newtypeCeo :: Newtype Ceo _
--
--newtype Janitor = Janitor Person2
--derive instance newtypeJanitor :: Newtype Janitor _
--
--genericPersonHasAddress :: ∀ a. Newtype a Person2 => a -> Address
--genericPersonHasAddress wrappedPerson =
--  getAddress $ unwrap wrappedPerson
--
--instance hasAddressCeo :: HasAddress Ceo where
--  getAddress = genericPersonHasAddress
--
--instance hasAddressJanitor :: HasAddress Janitor where
--  getAddress = genericPersonHasAddress

newtype Ceo = Ceo Person2
derive instance newtypeCeo :: Newtype Ceo _
derive newtype instance hasAddressCeo :: HasAddress Ceo

newtype Janitor = Janitor Person2
derive instance newtypeJanitor :: Newtype Janitor _
derive newtype instance hasAddressJanitor :: HasAddress Janitor

test :: Effect Unit
test = do
  log $ show $ name $ Person {firstName: "John", lastName: "Doe"}
  log $ show $ name $ Company "Doe inc."
  log $ show sx