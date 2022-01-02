{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( FnToken (..),
    CustomerInvoice (..),
    SupplierInvoice (..),
    FnError (..),
    getInvoices,
    getAccessToken,
    ClientSecret,
    AuthCode,
    AccessToken,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as S8
import Data.Time.Calendar (Day)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types

-- | An invoice to a customer
data CustomerInvoice = CustomerInvoice
  { -- | The Reference number
    cNumber :: String,
    -- | Balance of the invoice in cCurrency
    cBalance :: Float,
    -- | Name of the customer
    cName :: String,
    -- | Due Date
    cDueDate :: Day,
    -- |  Currency of the Invoice
    cCurrency :: String
  }
  deriving (Eq, Show)

instance FromJSON CustomerInvoice where
  parseJSON (Object o) =
    CustomerInvoice <$> o .: "DocumentNumber"
      <*> o .: "Balance"
      <*> o .: "CustomerName"
      <*> o .: "DueDate"
      <*> o .: "Currency"
  parseJSON invalid = typeMismatch "Customer Invoice" invalid

-- | A wrapper of lists of invoices used for parsing
newtype CustomerInvoices = CustomerInvoices {getCInv :: [CustomerInvoice]} deriving (Show)

instance FromJSON CustomerInvoices where
  parseJSON (Object o) = CustomerInvoices <$> o .:? "Invoices" .!= []
  parseJSON invalid = typeMismatch "List of customer invoices" invalid

-- | An Invoice from a supplier
data SupplierInvoice = SupplierInvoice
  { -- | The Reference number
    sNumber :: String,
    -- | Total of the invoice in sCurrency
    sTotal :: Float,
    -- | Name of the Supplier
    sName :: String,
    -- | Due Date
    sDueDate :: Day,
    -- | Currency of the Invoice
    sCurrency :: String
  }
  deriving (Eq, Show)

instance FromJSON SupplierInvoice where
  parseJSON (Object o) =
    SupplierInvoice <$> o .: "GivenNumber"
      <*> (read <$> (o .: "Total"))
      <*> o .: "SupplierName"
      <*> o .: "DueDate"
      <*> o .: "Currency"
  parseJSON invalid = typeMismatch "Supplier Invoice" invalid

-- | A wrapper of lists of invoices used for parsing
newtype SupplierInvoices = SupplierInvoices {getSInv :: [SupplierInvoice]} deriving (Show)

instance FromJSON SupplierInvoices where
  parseJSON (Object o) = SupplierInvoices <$> o .:? "SupplierInvoices" .!= []
  parseJSON invalid = typeMismatch "List of supplier invoices" invalid

-- | Get all unpaid invoices
getInvoices ::
  FnToken -> IO (Either FnError ([CustomerInvoice], [SupplierInvoice]))
getInvoices token = do
  (cResp, sResp) <-
    concurrently
      (sendRequest clientInvReq)
      (sendRequest suppliInvReq)
  return $ (,) <$> (getCInv <$> cResp) <*> (getSInv <$> sResp)
  where
    clientInvReq =
      addAccessToken token . setRequestPath "/3/invoices/?filter=unpaidoverdue"
    suppliInvReq =
      addAccessToken token
        . setRequestPath "/3/supplierinvoices/?filter=unpaidoverdue"

addAccessToken :: FnToken -> (Request -> Request)
addAccessToken token =
  addRequestHeader "Access-Token" (S8.pack $ accessToken token)
    . addRequestHeader "Client-Secret" (S8.pack $ clientSecret token)

sendRequest :: FromJSON a => (Request -> Request) -> IO (Either FnError a)
sendRequest reqLense = do
  eresp <- try $ httpJSONEither (reqLense baseReq') -- `catches` fnErrHandler
  case eresp of
    Left e -> return $ Left $ ConnectFail e
    Right resp -> do
      let respStatus = getResponseStatus resp
      return $ case statusCode respStatus of
        200 -> first ParseFail $ getResponseBody resp
        _ -> Left $ HttpError $ S8.unpack $ statusMessage respStatus
  where
    baseReq' =
      setRequestMethod "GET" $
        setRequestHeaders headers $
          setRequestSecure True $
            setRequestPort 443 $
              parseRequest_ "http://api.fortnox.se"
    headers =
      [("Accept", "application/json"), ("Content-type", "application/json")]

-- | Secret accociated with the app
type ClientSecret = String

-- | Code for requesting an AccessToken
type AuthCode = String

-- | Token Supplied by fortnox
type AccessToken = String

-- | FnToken contains whats needed to talk to the API
data FnToken = FnToken
  { accessToken :: AccessToken,
    clientSecret :: ClientSecret
  }
  deriving (Generic, Eq, Show)

instance FromJSON FnToken

instance ToJSON FnToken

newtype AuthReply = AuthReply {getAuth :: AccessToken} deriving (Show)

instance FromJSON AuthReply where
  parseJSON (Object o) = AuthReply <$> (o .: "Authorization" >>= (.: "AccessToken"))
  parseJSON invalid = typeMismatch "Authorization reply" invalid

-- | Generate a new access token, can only be run once for each AuthCode
getAccessToken :: AuthCode -> ClientSecret -> IO (Either FnError FnToken)
getAccessToken authCode secret = do
  resp <-
    sendRequest $
      setRequestPath "/3/invoices"
        . addRequestHeader "Authorization-Code" (S8.pack authCode)
        . addRequestHeader "Client-Secret" (S8.pack secret)
  let token = flip FnToken secret . getAuth <$> resp
  return token

data FnError = ConnectFail !HttpException | ParseFail !JSONException | HttpError String deriving (Show)

instance Exception FnError
