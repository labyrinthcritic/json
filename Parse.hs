module Parse where

import Control.Applicative
import Data.Char (isDigit, isSpace)

newtype Parser i o = Parser { parse :: [i] -> Maybe (o, [i]) }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser parse where
    parse input = do
      (output, rest) <- p input
      Just (f output, rest)

instance Applicative (Parser i) where
  pure a = Parser $ \input -> Just (a, input)

  Parser f <*> Parser p = Parser parse where
    parse input = do
      (f', rest) <- f input
      (output, rest) <- p rest
      Just (f' output, rest)

instance Monad (Parser i) where
  return = pure

  Parser p >>= k = Parser parse' where
    parse' input = do
      (output, rest) <- p input
      parse (k output) rest

instance Alternative (Parser i) where
  empty = Parser $ const empty

  Parser l <|> Parser r = Parser parse where
    parse input = l input <|> r input

-- Make a 'many' parser fail if it does not produce any output.
assert :: Parser i [o] -> Parser i [o]
assert parser = Parser $ \input ->
  case parse parser input of
    Nothing             -> Nothing
    Just ([], rest)     -> Nothing
    Just (output, rest) -> Just (output, rest)

pull :: (i -> Bool) -> Parser i i
pull predicate = Parser parse where
  parse input = case input of
    []                 -> Nothing -- eof
    x:xs | predicate x -> Just (x, xs)
    _                  -> Nothing

pullMany :: (i -> Bool) -> Parser i [i]
pullMany predicate = assert $ many (pull predicate)

unit :: Eq i => i -> Parser i i
unit i = pull (== i)

units :: Eq i => [i] -> Parser i [i]
units = traverse unit

separated :: Parser i a -> Parser i o -> Parser i [o]
separated by parser = (:) <$> parser <*> many (by *> parser) <|> pure []

data Value = Null
           | Number Int
           | String String
           | Bool Bool
           | Array [Value]
           | Object [(String, Value)]
           deriving (Show)

ws :: Parser Char String
ws = many (pull isSpace)

value :: Parser Char Value
value = nul <|> number <|> (String <$> string) <|> bool <|> array <|> object

nul :: Parser Char Value
nul = const Null <$> units "null"

number :: Parser Char Value
number = (Number . read) <$> pullMany isDigit

string :: Parser Char String
string = (unit '"' *> many (pull (/= '"')) <* unit '"')

bool :: Parser Char Value
bool = f <$> (units "false" <|> units "true") where
  f str = case str of
    "false" -> Bool False
    "true" -> Bool True

array :: Parser Char Value
array = Array <$> (left *> items <* right)
  where left  = ws *> unit '[' *> ws
        items = separated (ws *> unit ',' <* ws) value
        right = ws <* unit ']' <* ws

object :: Parser Char Value
object = Object <$> (left *> members <* right)
  where members = separated (ws *> unit ',' <* ws) member
        member  = (,) <$> (string <* ws <* unit ':' <* ws) <*> value
        left    = ws *> unit '{' *> ws
        right   = ws <* unit '}' <* ws
