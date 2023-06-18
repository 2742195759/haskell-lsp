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

module Lib
    ( Message(..)
    , SomeMessage(..)
    , handleSomeMessage
    , createErrorMessage
    ) where

import qualified GHC.Generics as G
import qualified Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as Vector
import Debug.Trace (trace)
import Data.Text.Encoding (encodeUtf8)

class (Show m, JSON.FromJSON (Param m), JSON.ToJSON(Result m)) => Message m where
    type Param m = (r :: *) | r -> m
    type Result m = (r :: *) | r -> m
    decodeJSON :: JSON.Value -> Parser (Param m)
    decodeJSON v = JSON.parseJSON v

    printMessage :: m -> IO ()
    printMessage = print
    handle :: m -> Result m

    {-createSomeMessage :: Param m -> SomeMessage-}

data SomeMessage = forall m. (Message m) => SomeMessage m

instance JSON.FromJSON SomeMessage where
    parseJSON (JSON.Array v) = do
        let valueId = (v Vector.! 0)
        let (JSON.String messagetext) = (v Vector.! 1)
        id <- JSON.parseJSON valueId :: Parser Int
        let message = case JSON.decode (L.fromStrict . encodeUtf8 $ messagetext) :: Maybe JSON.Value of 
                        Just (JSON.Object x) -> x
                        _ -> error "decode error."
        method  <- message JSON..: "method"
        param   <- message JSON..: "param"
        case method of
            JSON.String "A" -> do
                param <- decodeJSON param :: Parser (Param MessageA)
                return $ SomeMessage $ MessageA id param
            JSON.String "B" -> do
                param <- decodeJSON param :: Parser (Param MessageB)
                return $ SomeMessage $ MessageB id param
            _ -> error "method not found."


-- logic for ErrorMessage
data ErrorMessage = ErrorMessage  String deriving Show
instance Message ErrorMessage  where
    type Param ErrorMessage = String
    type Result ErrorMessage = String
    handle (ErrorMessage message) = message

data VimCommand = VimCommand [String] deriving (Show, G.Generic)
instance JSON.ToJSON VimCommand

vimCommand :: String -> VimCommand
vimCommand s = VimCommand ["ex", s]

-- logic for Method A
data ParamA = ParamA {x :: Int, y :: Int} deriving (Show, G.Generic)
instance JSON.FromJSON ParamA
data ResultA = ResultA {id::Int, o::Int} deriving (Show, G.Generic)
instance JSON.ToJSON ResultA
data MessageA = MessageA Int ParamA deriving Show
instance Message MessageA where
    type Param MessageA = ParamA
    type Result MessageA = VimCommand
    handle (MessageA id (ParamA x y)) = vimCommand "echo 'this is haskell vim rpc.'"


-- logic for Method B
data ParamB = ParamB {x :: String, y :: String} deriving (Show, G.Generic)
instance JSON.FromJSON ParamB
data ResultB = ResultB {id::Int, o::String} deriving (Show, G.Generic)
instance JSON.ToJSON ResultB
data MessageB = MessageB Int ParamB deriving Show
instance Message MessageB where
    type Param MessageB = ParamB
    type Result MessageB = ResultB
    handle (MessageB id (ParamB x y)) = ResultB id (x ++ y)


handleSomeMessage :: SomeMessage -> L.ByteString
handleSomeMessage (SomeMessage m) = JSON.encode . handle $ m 

createErrorMessage :: String -> SomeMessage
createErrorMessage message = SomeMessage $ ErrorMessage message
