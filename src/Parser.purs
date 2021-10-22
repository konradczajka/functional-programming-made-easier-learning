module Parser where

import Prelude
import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array ((:))
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.Int (fromString)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|), fromNonEmpty)
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (uncons, fromCharArray, singleton)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicate, none)
import Effect (Effect)
import Effect.Console (log)

type ParserState a = Tuple String a

type ParseFunction e a = ParserError e => String -> Either e (ParserState a)

class ParserError e where
    eof :: e
    invalidChar :: String -> e

data PError
    = EOF
    | InvalidChar String
derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
    show = genericShow

instance pErrorMyError :: ParserError PError where
    eof = EOF
    invalidChar = InvalidChar

newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
    map f p = Parser $ \s -> map f <$> parse p s

instance applyParser :: Apply (Parser e) where
--    apply p1 p2 = Parser $ \s -> case parse p1 s of
--        Left err -> Left err
--        Right (Tuple s1 h) -> case parse p2 s1 of
--            Left err -> Left err
--            Right (Tuple s2 x) -> Right $ Tuple s2 (h x)
--    apply p1 p2 = Parser \s -> do
--        Tuple s1 f <- parse p1 s
--        Tuple s2 x <- parse p2 s1
--        pure $ Tuple s2 $ f x
    apply = ap

instance applicativeParser :: Applicative (Parser e) where
    pure x = Parser \s -> pure $ Tuple s x

instance bindParser :: Bind (Parser e) where
--    bind p f = Parser \s -> case parse p s of
--        Left err -> Left err
--        Right (Tuple s1 h) -> parse (f h) s1
    bind p f = Parser \s -> do
       Tuple s1 x <- parse p s
       parse (f x) s1

instance monadParser :: Monad (Parser e)

instance altParser :: Alt (Parser e) where
    alt p1 p2 = Parser \s -> case parse p1 s of
        Left _ -> parse p2 s
        Right x -> Right x

parse :: ∀ e a. ParserError e => Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

char :: ∀ e. Parser e Char
char = Parser \s -> case uncons s of
    Nothing           -> Left eof
    Just {head, tail} -> Right $ Tuple tail head

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = char >>= \c1 -> char >>= \c2 -> pure $ Tuple c1 c2

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = do
    c1 <- char
    c2 <- char
    pure $ Tuple c1 c2

threeCharsA :: ∀ e. Parser e String
threeCharsA = (\c1 c2 c3 -> fromCharArray [c1, c2, c3]) <$> char <*> char <*> char

threeCharsB :: ∀ e. Parser e String
threeCharsB = char >>= \c1 -> char >>= \c2 -> char >>= \c3 -> pure $ fromCharArray [c1, c2, c3]

threeChars :: ∀ e. Parser e String
threeChars = do
    c1 <- char
    c2 <- char
    c3 <- char
    pure $ fromCharArray [c1, c2, c3]

tenChars :: ∀ e. Parser e String
tenChars = fromCharArray <$> sequence (replicate 10 char)

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = char >>= \c ->
    if pred c then pure c else fail $ invalidChar expected

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString

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

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p

--atMost :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
--atMost n p
--    | n <= 0    = pure []
--    | otherwise = optional [] $ p >>= \c -> (c : _) <$> atMost (n - 1) p

atMost :: ∀ e a f
    . Unfoldable f
    => (a -> f a -> f a)
    -> Int
    -> Parser e a
    -> Parser e (f a)
atMost cons n p
    | n <= 0    = pure none
    | otherwise = optional none $ p
        >>= \c -> cons c <$> atMost cons (n - 1) p

atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost (:) n p

optional :: ∀ e a. a -> Parser e a -> Parser e a
optional default p = p <|> pure default

range :: ∀ e a f
    . Traversable f
    => Unfoldable f
    => Semigroup (f a)
    => (a -> f a -> f a)
    -> Int
    -> Int
    -> Parser e a
    -> Parser e (f a)
range cons min max p
    | min < 0
        || max <= 0
        || max < min = pure none
    | otherwise      = count min p >>= \cs -> (cs <> _) <$> atMost cons (max - min) p

range' :: ∀ e
    .  Int
    -> Int
    -> Parser e Char
    -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

newtype Year = Year Int
derive instance genericYear :: Generic Year _
instance showYear :: Show Year where
    show = genericShow

newtype Month = Month Int
derive instance genericMonth :: Generic Month _
instance showMonth :: Show Month where
    show = genericShow

newtype Day = Day Int
derive instance genericDay :: Generic Day _
instance showDay :: Show Day where
    show = genericShow

data DateFormat
    = YearFirst
    | MonthFirst
derive instance genericDateFormat :: Generic DateFormat _
instance showDateFormat :: Show DateFormat where
    show = genericShow

type DateParts =
    { year :: Year
    , month :: Month
    , day :: Day
    , format :: DateFormat
    }

yearFirst :: ∀ e. ParserError e => Parser e DateParts
yearFirst = do
    year <- Year <<< digitsToNum <$> count' 4 digit
    constChar '-'
    month <- Month <<< digitsToNum <$> range' 1 2 digit
    constChar '-'
    day <- Day <<< digitsToNum <$> range' 1 2 digit
    pure $ {year: year, month: month, day: day, format: YearFirst}

monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
   month <- Month <<< digitsToNum <$> range' 1 2 digit
   constChar '/'
   day <- Day <<< digitsToNum <$> range' 1 2 digit
   constChar '/'
   year <- Year <<< digitsToNum <$> count' 4 digit
   pure $ {year: year, month: month, day: day, format: MonthFirst}

date :: ∀ e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst

instance lazyParser :: Lazy (Parser e a) where
    defer :: (Unit -> Parser e a) -> Parser e a
    defer f = Parser \s -> parse (f unit) s

some :: ∀ a f m
    . Unfoldable f
    => Alt m
    => Applicative m
    => Lazy (m (f a))
    => (a -> f a -> f a)
    -> m a
    -> m (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p

many :: ∀ a f m
    . Unfoldable f
    => Alt m
    => Applicative m
    => Lazy (m (f a))
    => (a -> f a -> f a)
    -> m a
    -> m (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure none

some' :: ∀ e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty (:) <$> some (:) p

many' :: ∀ e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many (:) p

digits :: ∀ e. ParserError e => Parser e String
digits = some' digit

constChar' :: ∀ e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (singleton c) (_ == c)

--(\d{1,4}), ([a-zA-Z ]+)([0-9]*)
ugly :: ∀ e. ParserError e => Parser e (Array String)
ugly = do
    g1 <- range' 1 4 digit
    constChar ','
    constChar ' '
    g2 <- some' (letter <|> constChar' ' ')
    g3 <- many' digit
    pure [g1, g2, g3]

uglyA :: ∀ e. ParserError e => Parser e (Array String)
uglyA =
    (\p1 p2 p3 -> [p1, p2, p3])
    <$> range' 1 4 digit
    <* constChar ','
    <* constChar ' '
    <*> some' (letter <|> constChar' ' ')
    <*> many' digit

uglyB :: ∀ e. ParserError e => Parser e (Array String)
uglyB =
    range' 1 4 digit
        >>= \p1 -> constChar ','
            >>= \_ -> constChar ' '
                >>= \_ -> some' (letter <|> constChar' ' ')
                    >>= \p2 -> many' digit
                        >>= \p3 -> pure [p1, p2, p3]

test :: Effect Unit
test = do
    log $ show $ (parse char "ABC" :: Either PError _)
    log $ show $ (parse twoCharsA "ABC" :: Either PError _)
    log $ show $ (parse threeCharsA "ABC" :: Either PError _)
    log $ show $ parse' char "ABC"
    log $ show $ parse' twoCharsA "ABC"
    log $ show $ parse' threeCharsA "ABC"
    log $ show $ parse' threeCharsA "A"
    log $ show $ parse' tenChars "ABCDEFGHIJKLM"
    log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
    log $ show $ parse' (count' 3 digit) "123456"
    log $ show $ parse' (count' 3 digit) "abc456"
    log $ show $ parse' (count' 4 letter) "Freddy"
    log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
    log $ show $ parse' (count' 10 alphaNum) "######"
    log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3"
    log $ show $ parse' (atMost' 2 alphaNum) "$_$"
    log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3"
    log $ show $ parse' yearFirst "1999-12-31"
    log $ show $ parse' monthFirst "12/31/1999"
    log $ show $ parse' (some' digit) "2343423423abc"
    log $ show $ parse' (many' digit) "_2343423423abc"
    log $ show $ parse' (some' digit) "_2343423423abc"
    log $ show $ parse' ugly "17, some words"
    log $ show $ parse' ugly "5432, some more words1234567890"