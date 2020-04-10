module Parse where

import           Data.UUID                     as UUID
import qualified Validate
import           Control.Monad
import           Text.Read

type Parser t = String -> Either String t
newtype Parse t = Parse { runParser :: Parser t }

instance Functor Parse where
  fmap f p = Parse (fmap f . runParser p)

instance Semigroup (Parse a) where
  p1 <> p2 = Parse (\s -> runParser p1 s <> runParser p2 s)

label :: Parse t -> String -> Parse t
label p err = Parse $ \s -> case runParser p s of
  Right x -> Right x
  _       -> Left err

fromMaybe :: (String -> Maybe a) -> Parse a
fromMaybe f = Parse $ \s -> case f s of
  Nothing -> Left "parse.error"
  Just r  -> Right r

stringParse :: Parse String
stringParse = Parse Right

uuidParse :: Parse UUID
uuidParse = fromMaybe UUID.fromString `label` "parse.expected.uuid"

validated :: Parse a -> Validate.Validator a -> Parse a
validated p v = Parse (runParser p >=> Validate.validate v)

readParse :: (Read a) => Parse a
readParse = fromMaybe readMaybe `label` "parse.read"

intParse :: Parse Int
intParse = readParse `label` "parse.expected.integer"

floatParse :: Parse Float
floatParse = readParse `label` "parse.expected.float"
