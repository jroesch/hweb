{-# LANGUAGE TemplateHaskell #-}
module Main where

import Network.Wai (Application, Response, responseBuilder)
import Network.Wai.Handler.Warp -- exports "run"
import Network.HTTP.Types.Status
-- Construct a Builder from a String
import Blaze.ByteString.Builder.Char.Utf8 (fromString) -- import builder

import HWeb.JSON
-- makeLenses ''W.Request
--


simpleStringResponse :: String -> Response
simpleStringResponse s = responseBuilder status200 [] (fromString s)

main :: IO ()
main = do
    putStrLn "Starting server up on port 9000..."
    run 9000 application

application :: Application
application _ = return $ simpleStringResponse "Hello Class!"
