{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception
import Data.Aeson
import Data.Char (isSpace)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Images
import Lib
import Network.HTTP.Client.Conduit
import Options.Applicative
import System.Directory
import System.Environment (lookupEnv)
import System.Exit
import System.IO.Error
import Text.PrettyPrint.Boxes hiding ((<>))
import qualified Text.PrettyPrint.Boxes as B
  ( (<>),
  )

data ArgOpts = ArgOpts
  { formatForBB :: Bool,
    authParams :: Maybe AuthCode
  }
  deriving (Show)

argOpts :: Parser ArgOpts
argOpts =
  ArgOpts
    <$> switch
      ((long "swiftbar" <> short 's') <> help "Format the output for swiftbar")
    <*> optional
      ( strOption
          ( long "auth" <> metavar "AUTHCODE"
              <> help
                "Get a new token using a auth code from the website"
          )
      )

main :: IO ()
main = do
  secFP <- clientSecretFP
  opts <-
    execParser $
      info
        (argOpts <**> helper)
        ( fullDesc
            <> header "noxstatus - get invoice status from fortnox.se"
            <> footer
              ( "A Fortnox client secret must be set from either "
                  <> "the envar $NOX_CLIENT_SECRET or in a file: \""
                  <> secFP
                  <> "\"."
              )
        )

  case authParams opts of
    Just ac -> newTokenFile ac
    Nothing -> do
      token <- loadTokenFile
      cDay <- currentDay
      res <- getInvoices token
      res' <- case res of
        Left (ConnectFail (HttpExceptionRequest _ (ConnectionFailure _))) ->
          if formatForBB opts
            then exitFailure
            else die "Error: Connection failed"
        Left e -> die $ "Error: \n" <> show e
        Right r -> return r
      putStr $ makeTable (formatForBB opts) cDay res'
      return ()

makeTable :: Bool -> Day -> ([CustomerInvoice], [SupplierInvoice]) -> String
makeTable forBB today (cus, sup) =
  if forBB && nCus + nSup < 1
    then []
    else barItem <> render table
  where
    nCus = length cus
    nSup = length sup
    diffDays' = text . show . diffDays today
    daysC =
      cWithLabels
        left
        [ (diffDays' . cDueDate <$> cus, "Days:"),
          (diffDays' . sDueDate <$> sup, "Days:")
        ]
    custC =
      cWithLabels
        left
        [(text . cName <$> cus, "Customer:"), (text . sName <$> sup, "Supplier:")]
    balC =
      cWithLabels
        right
        [ (text . show . cBalance <$> cus, "Sum:"),
          (text . show . sTotal <$> sup, "Sum:")
        ]
    currC =
      cWithLabels
        left
        [(text . cCurrency <$> cus, ""), (text . sCurrency <$> sup, "")]
    balC' = hsep 1 top [balC, currC]
    fontStr = "| font=Courier size=14"
    linkStr = " href=http://fortnox.se"
    formatC =
      if forBB
        then
          cWithLabels
            left
            [ (text (fontStr <> linkStr) <$ cus, fontStr),
              (text (fontStr <> linkStr) <$ sup, fontStr)
            ]
        else nullBox
    table = hsep 2 top [daysC, custC, balC'] B.<> formatC
    barItem =
      if forBB
        then
          unlines
            [ show nCus
                <> "/"
                <> show nSup
                <> "| templateImage="
                <> fnIcon
                <> " size=16 font=Courier",
              "---"
            ]
        else
          unlines
            [ "Unpaid customer invoices: " <> show nCus,
              "Unpaid supplier invoices: " <> show nSup,
              ""
            ]
    cWithLabels itemA bss = vcat left $ subCol <$> bss
      where
        subCol (subItems, label) =
          if null subItems then nullBox else text label // vcat itemA subItems

currentDay :: IO Day
currentDay =
  localDay <$> liftA2 utcToLocalTime getCurrentTimeZone getCurrentTime

cfgPath :: String -> IO FilePath
cfgPath fn = fmap (<> fn) $ getXdgDirectory XdgConfig $ "noxstatus" <> "/"

clientSecretFP :: IO FilePath
clientSecretFP = cfgPath "client_secret"

-- Retreive ClientSecret, either from ENVAR
-- or file.
getClientSecret :: IO String
getClientSecret = do
  secFP <- clientSecretFP
  mEnvSec <- lookupEnv "NOX_CLIENT_SECRET"
  mFileSec <- fromFile secFP
  let errMes = die $ "Error: No $NOX_CLIENT_SECRET envar or client secret file at: \"" <> secFP <> "\""
  maybe errMes pure $ mEnvSec <|> mFileSec
  where
    fromFile :: FilePath -> IO (Maybe String)
    fromFile fp = do
      se <- try $ readFile fp
      case se of
        Left (e :: IOError) ->
          if isDoesNotExistError e
            then pure Nothing
            else die $ "Error: Could not read client secret file: " <> fp
        Right s -> pure $ Just $ strip s

tokenPath :: IO FilePath
tokenPath = cfgPath "fn_token.json"

loadTokenFile :: IO FnToken
loadTokenFile = do
  tp <- tokenPath
  cfgFile <-
    eitherDecodeFileStrict' tp
      `catch` ( \e ->
                  if isDoesNotExistError e
                    then
                      die
                        "Error: Could not find token file, generate one with \"--auth\" argument"
                    else die $ "Error opening token file: \n" <> show e
              )

  case cfgFile of
    Left e -> die $ "Error parsing token file \"" <> tp <> "\":\n" <> e
    Right c -> return c

newTokenFile :: AuthCode -> IO ()
newTokenFile ac = do
  exists <- doesFileExist =<< tokenPath
  if exists
    then tokenPath >>= die . ("Token file already exists at: " <>)
    else do
      cs <- getClientSecret
      getAccessToken ac cs >>= \case
        Left e -> die $ "Error getting token file:\n" <> show e
        Right t -> do
          tp <- tokenPath
          encodeFile tp t
          putStrLn $ "Saved tokenfile to:\n" <> tp

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse
