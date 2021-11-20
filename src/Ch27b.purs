module Ch27b where

import Prelude
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept, runExceptT)
import Control.Parallel (parSequence, parallel, sequential)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign (F, Foreign, MultipleErrors)
import Foreign.Generic (defaultOptions, decodeJSON, encodeJSON, genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode)
import Foreign.Generic.Class (class Encode, class Decode)
import Foreign.JSON (parseJSON)
import Type.Proxy (Proxy(..))

newtype Centimeters = Centimeters Number

derive instance genericCentimeters :: Generic Centimeters _
instance showCentimeters :: Show Centimeters where
  show = genericShow
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeCentimeters :: Decode Centimeters
-- derive newtype instance decodeCentimeters :: Decode Centimeters
-- instance encodeCentimeters :: Encode Centimeters where
--   encode = genericEncode defaultOptions

newtype Kilograms = Kilograms Number

derive instance genericKilograms :: Generic Kilograms _
instance showKilograms :: Show Kilograms where
  show = genericShow
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeKilograms :: Decode Kilograms

newtype Years = Years Int

derive instance genericYears :: Generic Years _
instance showYears :: Show Years where
  show = genericShow
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeYears :: Decode Years

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
instance decodePersonal :: Decode Personal where
  decode = genericDecode defaultOptions

newtype GPA = GPA Number

derive instance genericGPA :: Generic GPA _
instance showGPA :: Show GPA where
  show = genericShow
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeGPA :: Decode GPA

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int

derive instance genericGrade :: Generic Grade _
instance showGrade :: Show Grade where
  show = genericShow
instance encodeGrade :: Encode Grade where
  encode = genericEncode defaultOptions
instance decodeGrade :: Decode Grade where
  decode = genericDecode defaultOptions

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
instance decodeStudent :: Decode Student where
  decode = genericDecode defaultOptions

data TeachingStatus = StudentTeacher | Probationary | NonTenured | Tenured

derive instance genericTeachingStatus :: Generic TeachingStatus _
instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow
instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions
instance decodeTeachingStatus :: Decode TeachingStatus where
  decode = genericDecode defaultOptions

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
instance decodeTeacher :: Decode Teacher where
  decode = genericDecode defaultOptions

processAjaxResult
  :: ∀ a
  . Show a
  => Decode a
  => Proxy a
  -> Either Ajax.Error (Ajax.Response String)
  -> String
processAjaxResult _ = case _ of
  Left err -> Ajax.printError err
  Right { body } -> case runExcept (decodeJSON body :: _ a) of
    Left err -> show err
    Right content -> show content

foreign import _reverseKeys :: Fn1 Foreign String
-- reverseKeys :: Foreign -> String
-- reverseKeys f = runFn1 _reverseKeys f

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
  results <- parSequence $ (\json -> Ajax.post ResponseFormat.string 
                        "http://localhost:3000/" 
                        $ Just $ RequestBody.String json)
        <$> [ encodeJSON testTeacher, encodeJSON testStudent ] 
  log $ case map (_.body) <$> sequence results of
    Left err -> Ajax.printError err
    Right [teacherJson, studentJson] ->
          show (processJSON teacherJson :: _ Teacher) <> "\n\n"
      <> show (processJSON studentJson :: _ Student)
    Right _ ->
      "The number of Ajax calls is different than what's being processed."
  where
    processJSON :: ∀ a. Decode a => String -> Either MultipleErrors a
    processJSON json = runExcept do
      o <- parseJSON json
      decodeJSON $ _reverseKeys o
-- test :: Effect Unit
-- test = launchAff_ do
--   result <- Ajax.post ResponseFormat.json
--     "http://localhost:3000/"
--     $ Just $ RequestBody.String $ encodeJSON testTeacher
--   log $ show $ bimap Ajax.printError (stringify <<< _.body) $ result
--   log $ processAjaxResult (Proxy :: _ Teacher) result
