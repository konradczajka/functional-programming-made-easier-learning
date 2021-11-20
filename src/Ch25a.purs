module Ch25a where

import Prelude

import Affjax (printError)
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Generic (decodeJSON, encodeJSON, genericDecode, genericEncode, defaultOptions)
import Foreign.Generic.Class (class Decode ,class Encode, Options, SumEncoding(..))

newtype Centimeters = Centimeters Number

derive instance genericCentimeters :: Generic Centimeters _
instance showCentimeters :: Show Centimeters where
  show = genericShow
derive newtype instance encodeCentimeters :: Encode Centimeters
-- derive newtype instance decodeCentimeters :: Decode Centimeters
-- instance encodeCentimeters :: Encode Centimeters where
--   encode = genericEncode defaultOptions
decodeOptions :: Options
decodeOptions = defaultOptions
  { sumEncoding = TaggedObject
    { tagFieldName: "gat"
    , contentsFieldName: "stnetnoc"
    , constructorTagTransform: identity
    }
    ,
    unwrapSingleConstructors = true
  }
instance decodeCentimeters :: Decode Centimeters where
  decode = genericDecode decodeOptions

newtype Kilograms = Kilograms Number

derive instance genericKilograms :: Generic Kilograms _
instance showKilograms :: Show Kilograms where
  show = genericShow
derive newtype instance encodeKilograms :: Encode Kilograms
instance decodeKilograms :: Decode Kilograms where
  decode = genericDecode decodeOptions

newtype Years = Years Int

derive instance genericYears :: Generic Years _
instance showYears :: Show Years where
  show = genericShow
derive newtype instance encodeYears :: Encode Years
instance decodeYears :: Decode Years where
  decode = genericDecode decodeOptions

type Personal =
  { height :: Centimeters
  , weight :: Kilograms
  , age :: Years
  }
type ReversedPersonal =
  { thgieh :: Centimeters
  , thgiew :: Kilograms
  , ega :: Years
  }

newtype GPA = GPA Number

derive instance genericGPA :: Generic GPA _
instance showGPA :: Show GPA where
  show = genericShow
derive newtype instance encodeGPA :: Encode GPA
instance decodeGPA :: Decode GPA where
  decode = genericDecode decodeOptions

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int

derive instance genericGrade :: Generic Grade _
instance showGrade :: Show Grade where
  show = genericShow
instance encodeGrade :: Encode Grade where
  encode = genericEncode defaultOptions
instance decodeGrade :: Decode Grade where
  decode = genericDecode decodeOptions

type Student =
  { grade :: Grade
  , teacher :: Teacher
  , gpa :: GPA
  , personal :: Personal
  }
type ReversedStudent =
  { edarg :: Grade
  , rehcaet :: ReversedTeacher
  , apg :: GPA
  , lanosrep :: ReversedPersonal
  }

data TeachingStatus = Student | Probationary | NonTenured | Tenured

derive instance genericTeachingStatus :: Generic TeachingStatus _
instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow
instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions
instance decodeTeachingStatus :: Decode TeachingStatus where
  decode = genericDecode decodeOptions

type Teacher =
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }
type ReversedTeacher = 
  { sedarg :: Array Grade
  , stnedutSfOrebmun :: Int
  , lanosrep :: ReversedPersonal
  , sutats :: TeachingStatus
  }

teacher :: Teacher
teacher =
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal: {
    height: Centimeters 162.56
    , weight: Kilograms 63.5
    , age: Years 31
    }
  , status: NonTenured
  }
test :: Effect Unit
test = launchAff_ do
  result <- Ajax.post ResponseFormat.string 
    "http://localhost:3000/" 
    $ Just $ RequestBody.String $ encodeJSON teacher
  log $ case result of
    Left err -> printError err
    Right { body } -> case runExcept (decodeJSON body :: _ ReversedTeacher) of
      Left err -> show err
      Right reversedTeacher -> show reversedTeacher
  -- pure unit
  -- log $ show $ bimap Ajax.printError _.body result