module Ch25b where

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Control.Parallel (parSequence, parallel, sequential)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson, stringify, (.:))
import Data.Argonaut.Decode.Decoders (decodeJObject)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Traversable (sequence)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Generic (defaultOptions, genericEncode, encodeJSON)
import Foreign.Generic.Class (class Decode, class Encode)
import Type.Proxy (Proxy(..))

newtype Centimeters = Centimeters Number

derive instance genericCentimeters :: Generic Centimeters _
instance showCentimeters :: Show Centimeters where
  show = genericShow
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeJsonCentimeters :: DecodeJson Centimeters
-- derive newtype instance decodeCentimeters :: Decode Centimeters
-- instance encodeCentimeters :: Encode Centimeters where
--   encode = genericEncode defaultOptions

newtype Kilograms = Kilograms Number

derive instance genericKilograms :: Generic Kilograms _
instance showKilograms :: Show Kilograms where
  show = genericShow
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeJsonKilograms :: DecodeJson Kilograms

newtype Years = Years Int

derive instance genericYears :: Generic Years _
instance showYears :: Show Years where
  show = genericShow
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeJsonYears :: DecodeJson Years

newtype Personal = Personal
  { height :: Centimeters
  , weight :: Kilograms
  , age :: Years
  }

derive instance genericPersonal :: Generic Personal _
instance showPersonal :: Show Personal where
  show = genericShow
instance encodePersonal :: Encode Personal where
  encode = genericEncode defaultOptions
instance decodeJsonPersonal :: DecodeJson Personal where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Personal" then do
      c <- o .: "stnetnoc"
      height <- c .: "thgieh"
      weight <- c .: "thgiew"
      age <- c .: "ega"
      pure $ Personal { height, weight, age }
    else Left $ AtKey "tag" $ UnexpectedValue json

newtype GPA = GPA Number

derive instance genericGPA :: Generic GPA _
instance showGPA :: Show GPA where
  show = genericShow
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeJsonGPA :: DecodeJson GPA

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int

derive instance genericGrade :: Generic Grade _
instance showGrade :: Show Grade where
  show = genericShow
instance encodeGrade :: Encode Grade where
  encode = genericEncode defaultOptions
instance decodeJsonGrade :: DecodeJson Grade where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    let contents :: ∀ a. DecodeJson a => Either JsonDecodeError a
        contents = o .: "stnetnoc"
    case tag of
      "Preschool" -> pure Preschool
      "Kindergarten" -> pure Kindergarten
      "Grade" -> Grade <$> contents
      "High" -> High <$> contents
      "College" -> College <$> contents
      _ -> Left $ AtKey "tag" $ UnexpectedValue json

newtype Student = Student
  { grade :: Grade
  , teacher :: Teacher
  , gpa :: GPA
  , personal :: Personal
  }

derive instance genericStudent :: Generic Student _
instance showStudent :: Show Student where
  show = genericShow
instance encodeStudent :: Encode Student where
  encode = genericEncode defaultOptions
instance decodeJsonStudent :: DecodeJson Student where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Student" then do
      c <- o .: "stnetnoc"
      grade <- c .: "edarg"
      teacher <- c .: "rehcaet"
      gpa <- c .: "apg"
      personal <- c .: "lanosrep"
      pure $ Student { grade, teacher, gpa, personal }
    else Left $ AtKey "tag" $ UnexpectedValue json

data TeachingStatus = StudentTeacher | Probationary | NonTenured | Tenured

derive instance genericTeachingStatus :: Generic TeachingStatus _
instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow
instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions
instance decodeJsonTeachingStatus :: DecodeJson TeachingStatus where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    case tag of
      "StudentTeacher" -> pure StudentTeacher
      "Probationary" -> pure Probationary
      "NonTenured" -> pure NonTenured
      "Tenured" -> pure Tenured
      _ -> Left $ AtKey "tag" $ UnexpectedValue json

newtype Teacher = Teacher
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }

derive instance genericTeacher :: Generic Teacher _
instance showTeacher :: Show Teacher where
  show = genericShow
instance encodeTeacher :: Encode Teacher where
  encode = genericEncode defaultOptions
instance decodeJsonTeacher :: DecodeJson Teacher where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Teacher" then do
      c <- o .: "stnetnoc"
      grades <- c .: "sedarg"
      numberOfStudents <- c .: "stnedutSfOrebmun"
      personal <- c .: "lanosrep"
      status <- c .: "sutats"
      pure $ Teacher { grades, numberOfStudents, personal, status }
    else Left $ AtKey "tag" $ UnexpectedValue json

processAjaxResult
  :: ∀ a
  . Show a
  => DecodeJson a
  => Proxy a
  -> Either Ajax.Error (Ajax.Response Json)
  -> String
processAjaxResult _ = case _ of
  Left err -> Ajax.printError err
  Right { body } -> case (decodeJson body :: _ a) of
    Left err -> show err
    Right content -> show content

testTeacher :: Teacher
testTeacher = Teacher
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal: Personal {
      height: Centimeters 162.56
    , weight: Kilograms 63.5
    , age: Years 31
    }
  , status: NonTenured
  }

testStudent :: Student
testStudent = Student
  { grade: Grade 1
  , teacher: testTeacher
  , gpa: GPA 3.2
  , personal: Personal {
      height: Centimeters 107.9
    , weight: Kilograms 17.9
    , age: Years 5
    }
  }

test :: Effect Unit
test = launchAff_ do
  results <- parSequence $ (\json -> Ajax.post ResponseFormat.json 
                        "http://localhost:3000/" 
                        $ Just $ RequestBody.String json)
        <$> [ encodeJSON testTeacher, encodeJSON testStudent ]
  log $ case map (_.body) <$> sequence results of
    Left err -> Ajax.printError err
    Right [teacherJson, studentJson] ->
      show (decodeJson teacherJson :: _ Teacher) <> "\n\n"
      <> show (decodeJson studentJson :: _ Student)
    Right _ ->
      "The number of Ajax calls is different than what's being processed."
  pure unit
-- test :: Effect Unit
-- test = launchAff_ do
--   result <- Ajax.post ResponseFormat.json
--     "http://localhost:3000/"
--     $ Just $ RequestBody.String $ encodeJSON testTeacher
--   log $ show $ bimap Ajax.printError (stringify <<< _.body) $ result
--   log $ processAjaxResult (Proxy :: _ Teacher) result
