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

json :: Parser Value
json = (Object <$> object) 
     <|> (Array <$> array)

value :: Parser Value
value =  (Object <$> object)
     <|> (Array <$> array)
     <|> (String <$> str)
     <|> (Number <$> number)
     <|> (const Null <$> nullP)

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

number = undefined
str = undefined

-- javascript shit right hurr #YOLO
nullP = undefined
