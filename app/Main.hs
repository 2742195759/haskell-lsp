{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}


module Main (main) where

import Lib (Message(..), SomeMessage(..), handleSomeMessage, createErrorMessage)
import SelectLib (startSelect
                , SelectEnv(..)
                , Handle(..)
                , echoHandle
                , selectInsert
                , SelectMonad(..)
                , selectRemove
                )
import Data.Aeson as JSON
import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString (send)
import Control.Monad.Trans.Class (lift)
import Network.StreamSocket
import Network.Stream (readLine)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BS

readLine' :: Socket -> IO String
readLine' sock = do 
    line <- readLine sock
    case line of
        Left _ -> E.throw $ E.AssertionFailed "Error happen in readLine."
        Right s -> return s


getline :: Socket -> SelectMonad BS.ByteString
getline sock = do
    line <- lift . (fmap BS.fromStrict) . (fmap C8.pack) . readLine' $ sock
    if line == "" then selectRemove sock >> return ""
    else return line
    

vimHandle :: Handle
vimHandle sock = do 
    line <- getline sock
    let message = JSON.decode line :: Maybe SomeMessage
    let k = case message of
                Nothing -> createErrorMessage "decode error."
                Just x -> x
    let output = C8.unpack . BS.toStrict . handleSomeMessage $ k
    lift $ send sock (C8.pack (output ++ "\n"))
    return ()
    

listenHandle :: Handle
listenHandle sock = do
    (conn, _) <- lift $ accept sock
    selectInsert conn vimHandle
    {-selectInsert conn echoHandle-}


initHandle :: SelectMonad ()
initHandle = do
    return ()

main :: IO ()
main = startSelect initHandle listenHandle
