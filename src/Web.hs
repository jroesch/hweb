{-# LANGUAGE TemplateHaskell #-}
module Main where

import Network.Wai (Application, Response, responseBuilder)
import Network.Wai.Handler.Warp 
import Network.HTTP.Types.Status
-- Construct a Builder from a String
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

-- makeLenses ''W.Request
--

simpleStringResponse :: String -> Response
simpleStringResponse s = responseBuilder status200 [] (fromString s)

main :: IO ()
main = do
    putStrLn "Starting server up on port 9000..."
    run 9000 application

application :: Application
application request = return $ simpleStringResponse "Hello World!"
