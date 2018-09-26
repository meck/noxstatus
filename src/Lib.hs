{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( FnToken(..)
  , CustomerInvoice(..)
  , SupplierInvoice(..)
  , FnError(..)
  , getInvoices
  , getAccessToken
  , ClientSecret
  , AuthCode
  , AccessToken
  )
where

import           Control.Exception
import           Control.Concurrent.Async       ( concurrently )
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor                 ( first )
import           GHC.Generics
import           Data.Time.Calendar             ( Day )
import qualified Data.ByteString.Char8         as S8


-- | An invoice to a customer
data CustomerInvoice = CustomerInvoice
  { cNumber :: String   -- ^ The Reference number
  , cBalance :: Float   -- ^ Balance of the invoice in cCurrency
  , cName :: String     -- ^ Name of the customer
  , cDueDate :: Day     -- ^ Due Date
  , cCurrency :: String -- ^  Currency of the Invoice
  } deriving (Eq, Show)

instance FromJSON CustomerInvoice where
  parseJSON (Object o) =
    CustomerInvoice <$> o .: "DocumentNumber"
    <*> o .: "Balance"
    <*> o .: "CustomerName"
    <*> o .: "DueDate"
    <*> o .: "Currency"
  parseJSON invalid    = typeMismatch "Customer Invoice" invalid

-- | A wrapper of lists of invoices used for parsing
newtype CustomerInvoices =  CustomerInvoices { getCInv ::[CustomerInvoice] } deriving (Show)

instance FromJSON CustomerInvoices where
  parseJSON (Object o) = CustomerInvoices <$> o .:? "Invoices" .!= []
  parseJSON invalid    = typeMismatch "List of customer invoices" invalid

-- | An Invoice from a supplier
data SupplierInvoice = SupplierInvoice
  { sNumber :: String    -- ^ The Reference number
  , sTotal :: Float      -- ^ Total of the invoice in sCurrency
  , sName :: String      -- ^ Name of the Supplier
  , sDueDate :: Day      -- ^ Due Date
  , sCurrency :: String  -- ^ Currency of the Invoice
  } deriving (Eq, Show)

instance FromJSON SupplierInvoice where
  parseJSON (Object o) =
    SupplierInvoice <$> o .: "GivenNumber"
    <*> (read <$> (o .: "Total"))
    <*> o .: "SupplierName"
    <*> o .: "DueDate"
    <*> o .: "Currency"
  parseJSON invalid    = typeMismatch "Supplier Invoice" invalid

-- | A wrapper of lists of invoices used for parsing
newtype SupplierInvoices =  SupplierInvoices { getSInv :: [SupplierInvoice] }deriving (Show)

instance FromJSON SupplierInvoices where
  parseJSON (Object o) = SupplierInvoices <$> o .:? "SupplierInvoices" .!= []
  parseJSON invalid    = typeMismatch "List of supplier invoices" invalid

-- | Get all unpaid invoices
getInvoices
  :: FnToken -> IO (Either FnError ([CustomerInvoice], [SupplierInvoice]))
getInvoices token = do
  (cResp, sResp) <- concurrently (sendRequest clientInvReq)
                                 (sendRequest suppliInvReq)
  return $ (,) <$> (getCInv <$> cResp) <*> (getSInv <$> sResp)
 where
  clientInvReq = addAccessToken token . setRequestPath "/3/invoices"
  suppliInvReq = addAccessToken token . setRequestPath "/3/supplierinvoices"

addAccessToken :: FnToken -> (Request -> Request)
addAccessToken token =
  addRequestHeader "Access-Token" (S8.pack $ accessToken token)
    . addRequestHeader "Client-Secret" (S8.pack $ clientSecret token)

sendRequest :: FromJSON a => (Request -> Request) -> IO (Either FnError a)
sendRequest reqLense = do
  eresp <- try $ httpJSONEither (reqLense baseReq') -- `catches` fnErrHandler
  case eresp of
    Left  e    -> return $ Left $ ConnectFail e
    Right resp -> do
      let respStatus = getResponseStatus resp
      return $ case statusCode respStatus of
        200 -> first ParseFail $ getResponseBody resp
        _   -> Left $ HttpError $ S8.unpack $ statusMessage respStatus
 where
  baseReq' =
    setRequestMethod "GET"
      $ setRequestHeaders headers
      $ setRequestSecure True
      $ setRequestPort 443
      $ parseRequest_ "http://api.fortnox.se"
  headers =
    [("Accept", "application/json"), ("Content-type", "application/json")]

-- | Secret accociated with the app
type ClientSecret = String

-- | Code for requesting an AccessToken
type AuthCode = String

-- | Token Supplied by fortnox
type AccessToken = String

-- | FnToken contains whats needed to talk to the API
data FnToken = FnToken { accessToken :: AccessToken
                       , clientSecret :: ClientSecret }
                       deriving (Generic, Eq,Show)

instance FromJSON FnToken
instance ToJSON FnToken

newtype AuthReply = AuthReply { getAuth :: AccessToken } deriving Show

instance FromJSON AuthReply where
  parseJSON (Object o) = AuthReply <$> (o.: "Authorization" >>=  (.: "AccessToken"))
  parseJSON invalid    = typeMismatch "Authorization reply" invalid

-- | Generate a new access token, can only be run once for each AuthCode
getAccessToken :: AuthCode -> ClientSecret -> IO (Either FnError FnToken)
getAccessToken authCode secret = do
  resp <-
    sendRequest
    $ setRequestPath "/3/invoices"
    . addRequestHeader "Authorization-Code" (S8.pack authCode)
    . addRequestHeader "Client-Secret"      (S8.pack secret)
  let token = flip FnToken secret . getAuth <$> resp
  return token

data FnError = ConnectFail !HttpException | ParseFail !JSONException | HttpError String deriving (Show)

instance Exception FnError
