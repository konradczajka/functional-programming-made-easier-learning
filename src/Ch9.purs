module Ch9 where

import Prelude (Unit, class Eq, class Show, show, ($), (==), (&&), discard)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

class Semigroup a where
    append :: a -> a -> a

infixr 5 append as <>

class  Semigroup a <= Monoid a where
    mempty :: a

class Monoid a <= Group a where
    ginverse :: a -> a

data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _
instance showAndBool :: Show AndBool where
    show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
    append ATrue ATrue = ATrue
    append _ _ = AFalse
instance monoidAndBool :: Monoid AndBool where
    mempty = ATrue

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
    log "Verifying AndBool Semigroup Laws (1 test)"
    log $ show $ (AFalse <> ATrue) <> ATrue == AFalse <> (ATrue <> ATrue)

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
    log "Verifying AndBool Monoid Laws (2 tests)"
    log $ show $ ATrue <> mempty == ATrue && mempty <> ATrue == ATrue
    log $ show $ AFalse <> mempty == AFalse && mempty <> AFalse == AFalse

data OrBool = OFalse | OTrue
derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _
instance showOrBool :: Show OrBool where
    show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
    append OFalse OFalse = OFalse
    append _ _ = OTrue
instance monoidOrBool :: Monoid OrBool where
    mempty = OFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
    log "Verifying OrBool Semigroup Laws (1 test)"
    log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
    log "Verifying OrBool Monoid Laws (2 tests)"
    log $ show $ OTrue <> mempty == OTrue && mempty <> OTrue == OTrue
    log $ show $ OFalse <> mempty == OFalse && mempty <> OFalse == OFalse

data Mod4 = Zero | One | Two | Three
derive instance eqMod4 :: Eq Mod4
derive instance genericMod4 :: Generic Mod4 _
instance showMod4 :: Show Mod4 where
    show = genericShow

instance semigroupMod4 :: Semigroup Mod4 where
    append Zero x = x
    append x Zero = x

    append One One = Two
    append One Two = Three
    append One Three = Zero

    append Two One = Three
    append Two Two = Zero
    append Two Three = One

    append Three One = Zero
    append Three Two = One
    append Three Three = Two

instance monoidMod4 :: Monoid Mod4 where
    mempty = Zero

instance groupMod4 :: Group Mod4 where
    ginverse Zero = Zero
    ginverse One = Three
    ginverse Two = Two
    ginverse Three = One

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do
    log $ show $ One <> (Two <> Three) == (One <> Two) <> Three

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do
    log $ show $ Zero <> mempty == Zero && mempty <> Zero == Zero
    log $ show $ One <> mempty == One && mempty <> One == One
    log $ show $ Two <> mempty == Two && mempty <> Two == Two
    log $ show $ Three <> mempty == Three && mempty <> Three == Three

data Maybe a = Nothing | Just a
derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

newtype First a = First (Maybe a)
derive instance genericFirst :: Generic (First a) _
instance showFirst :: Show a => Show (First a) where
    show = genericShow

newtype Last a = Last (Maybe a)
derive instance genericLast :: Generic (Last a) _
instance showLast :: Show a => Show (Last a) where
    show = genericShow

instance semigroupFirst :: Semigroup (First a) where
    append (First Nothing) last = last
    append first _ = first

instance monoidFirst :: Monoid (First a) where
    mempty = First Nothing

instance semigroupLast :: Semigroup (Last a) where
    append first (Last Nothing) = first
    append _ last = last

instance monoidLast :: Monoid (Last a) where
    mempty = Last Nothing

test :: Effect Unit
test = do
    log $ show $ ATrue <> ATrue
    log $ show $ ATrue <> AFalse
    log $ show $ AFalse <> AFalse
    log $ show $ mempty <> ATrue == ATrue
    log $ show $ mempty <> AFalse == ATrue
    verifyAndBoolSemigroup
    verifyAndBoolMonoid
    verifyOrBoolSemigroup
    verifyOrBoolMonoid
    verifyMod4Semigroup
    verifyMod4Monoid
    log $ show $ First Nothing <> First (Just 77)
    log $ show $ Last (Just 1) <> Last (Just 99)
