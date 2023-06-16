{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}


module Main (main) where

import Lib
import Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import qualified GHC.Generics as G


data T = A | B


data M (t :: T) where
  MA :: M 'A
  MB :: M 'B

data SomeMethod = forall (t::T). SomeMethod (M t)
data SomeMessage = forall (t::T). SomeMessage (M t) (Param t)


instance JSON.FromJSON SomeMessage where
    parseJSON = withObject "SomeMessage" $ \v -> do
        method <- v .: "method"
        param <- v .: "param"
        SomeMethod m <- JSON.parseJSON method
        param <- (decodeParam m param)
        return $ SomeMessage m param


instance JSON.FromJSON SomeMethod where
    parseJSON (JSON.String "A") = return $ SomeMethod MA
    parseJSON (JSON.String "B") = return $ SomeMethod MB


-- value of (M t) -> value of (Param t)
{-decodeParam :: M t -> JSON.Value -> Parser (Param t)-}
{-decodeParam MA = JSON.parseJSON -}
{-decodeParam MB = JSON.parseJSON-}


data ParamA = ParamA {x :: Int, y :: Int} deriving (G.Generic, Show)
instance JSON.FromJSON ParamA  where {}


data ParamB = ParamB {x :: String, y :: String} deriving (G.Generic, Show)
instance JSON.FromJSON ParamB where {}


type family Param (t::T) where
    Param 'A = ParamA 
    Param 'B = ParamB 


handle :: M t -> Param t -> IO ()
handle MA (ParamA x y) = print $ x + y
handle MB (ParamB x y) = print $ x ++ y


func :: SomeMessage -> IO ()
func (SomeMessage s param) = handle s param


main :: IO ()
main = do 
    let stringB = "{\"method\": \"B\", \"param\": {\"x\": \"xxx\", \"y\": \"yyy\"}}"
    let stringA = "{\"method\": \"A\", \"param\": {\"x\": \"1\", \"y\": \"2\"}}"
    {-let reqs = [SomeMessage MA (ParamA 1 1), SomeMessage MB (ParamB "sdf" "xxx")]-}
    let message = JSON.decode stringB :: Maybe SomeMessage
    let k = case message of 
                Nothing -> error ("decode error." ++ show stringA)
                Just x -> x
    let reqs = [k]
    mconcat $ map func reqs


