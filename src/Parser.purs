module Parser where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Traversable (class Traversable, sequence)
import Data.Unfoldable (class Unfoldable, replicate, none)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

type ParserState a = Tuple String a

type ParseFunction e a = ParserError e => String -> Either e (ParserState a)

class ParserError e where
    eof :: e

data PError = EOF
derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
    show = genericShow

instance pErrorMyError :: ParserError PError where
    eof = EOF

newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
    map f p = Parser $ \s -> map f <$> parse p s

instance applyParser :: Apply (Parser e) where
    apply p1 p2 = Parser $ \s -> case parse p1 s of
        Left err -> Left err
        Right (Tuple s1 h) -> case parse p2 s1 of
            Left err -> Left err
            Right (Tuple s2 x) -> Right $ Tuple s2 (h x)

instance applicativeParser :: Applicative (Parser e) where
    pure x = Parser \s -> pure $ Tuple s x

parse :: ∀ e a. ParserError e => Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
    Nothing           -> Left eof
    Just {head, tail} -> Right $ Tuple tail head

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = Tuple <$> char <*> char

threeChars :: ∀ e. Parser e String
threeChars = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

tenChars :: ∀ e. Parser e String
tenChars = fromCharArray <$> sequence (replicate 10 char)

count
    :: ∀ e a f
    .  Unfoldable f
    => Traversable f
    => Int
    -> Parser e a
    -> Parser e (f a)
count n p
    | n <= 0    = pure none
    | otherwise = sequence (replicate n p)

test :: Effect Unit
test = do
    log $ show $ (parse char "ABC" :: Either PError _)
    log $ show $ (parse twoChars "ABC" :: Either PError _)
    log $ show $ (parse threeChars "ABC" :: Either PError _)
    log $ show $ parse' char "ABC"
    log $ show $ parse' twoChars "ABC"
    log $ show $ parse' threeChars "ABC"
    log $ show $ parse' threeChars "A"
    log $ show $ parse' tenChars "ABCDEFGHIJKLM"
    log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"