{-# LANGUAGE BangPatterns #-}
module HWeb.JSON where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.Text
import Control.Applicative ((<*), (*>), (<*>), (<$>))

data Value = Object !Object
           | Array !Array
           | String !T.Text
           | Number !Number
           | Bool   !Bool
           | Null

type Object = H.HashMap T.Text Value

type Array = V.Vector Value

data Number = I !Integer 
            | D !Double
            deriving (Eq, Show)

json :: Parser Value
json = (Object <$> object) 
     <|> (Array <$> array)

-- Focus here
value :: Parser Value
value =  (Object <$> object)
     <|> (Array <$> array)
     <|> (String <$> str)
     <|> (Number <$> number)
     <|> (Bool   <$> bool)
     <|> (const Null <$> nullP)

instance Show Value where
    show (Object obj) = show obj
    show (Array arr)  = show arr
    show (String str) = show str
    show (Number num) = show num
    show (Bool b)     = show b
    show Null         = "null"

object :: Parser Object
object = do
    char '{' <* spaces
    ps <- pair `sepBy` sep
    char '}'
    return $ H.fromList ps
  where sep = spaces >> char ',' >> spaces
        pair = do
          k <- key <* char ':'
          v <- value
          return (k, v)
        key = T.pack <$> between (char '"') (char '"') (many anyChar)

array :: Parser Array
array = do
    char '[' <* spaces
    vs <- value `sepBy` sep
    char ']'
    return $ V.fromList vs
  where sep = spaces >> char ',' >> spaces

number :: Parser Number
number = do
    num <- string "0" <|> nonzero
    return $ I (read num)
  where nonzero = do
          d  <- oneOf ['1'..'9']
          ds <- many digit
          return $ d:ds

str :: Parser T.Text
str = fmap T.pack (inQuotes $ many $ noneOf "\"")
  where inQuotes = between (char '"') (char '"')

nullP :: Parser String
nullP = string "null"

bool = trueP <|> falseP
  where trueP = const True <$> string "true"
        falseP = const False <$> string "false"
