{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Lib
import qualified ClientSecret                  as CS
import           Control.Exception
import           Data.Aeson
import           Text.PrettyPrint.Boxes  hiding ( (<>) )
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Data.Semigroup                 ( (<>) )
import           System.Exit
import           System.Directory
import           System.IO.Error
import           Options.Applicative
import           Network.HTTP.Client.Conduit

data ArgOpts = ArgOpts
  { formatForBB :: Bool
  , authParams:: Maybe AuthCode }
  deriving Show

argOpts :: Parser ArgOpts
argOpts =
  ArgOpts
    <$> switch
          ((long "bitbar" <> short 'b') <> help "Format the output for bitbar")
    <*> optional
          (strOption
            (long "auth" <> metavar "AUTHCODE" <> help
              "Get a new token using a auth code from the website"
            )
          )

main :: IO ()
main = do
  opts <- execParser $ info
    (argOpts <**> helper)
    (fullDesc <> header "noxstatus - get invoice status from fortnox.se")

  case authParams opts of
    Just ac -> newTokenFile ac
    Nothing -> do
      token <- loadTokenFile
      cDay  <- currentDay
      res   <- getInvoices token
      res'  <- case res of
        Left (ConnectFail (HttpExceptionRequest _ (ConnectionFailure _))) ->
          if formatForBB opts
            then exitFailure
            else die "Error: Connetion failed"
        Left  e -> die $ "Error: \n" <> show e
        Right r -> return r

      putStr $ getMenu (formatForBB opts) cDay res'
      return ()

getMenu :: Bool -> Day -> ([CustomerInvoice], [SupplierInvoice]) -> String
getMenu frmtBB d (cus, sup) = if nCus + nSup < 1 && frmtBB
  then ""
  else barItem <> table
 where
  nCus    = length cus
  nSup    = length sup
  barItem = if frmtBB
    then unlines ["Fortnox: " <> show nCus <> "/" <> show nSup, "---"]
    else unlines
      [ "Obetalda Kundfakt: " <> show (length cus)
      , "Obetalda Levfakt: " <> show (length sup)
      ]
  table  = addFont $ render $ hsep 2 left $ zipWith (/+/) cTable sTable
  cTable = if null cus
    then [nullBox]
    else getList d frmtBB "Kund:" cus cDueDate cName cBalance cCurrency
  sTable = if null sup
    then [nullBox]
    else getList d frmtBB "Lev:" sup sDueDate sName sTotal sCurrency
  addFont = if frmtBB
    then unlines . fmap (<> "| font=Courier size=14") . lines
    else id


getList
  :: (Show a1, Foldable f, Functor f)
  => Day
  -> Bool
  -> String
  -> f a2
  -> (a2 -> Day)
  -> (a2 -> String)
  -> (a2 -> a1)
  -> (a2 -> String)
  -> [Box]
getList day frmtBB label inv getDay getName getBalance getCurr =
  [colDays, colNames, colBal, colBitbar]
 where
  colDays =
    vcat left
      $ text "Dagar:"
      : [vcat right (text . show . diffDays day . getDay <$> inv)]
  colNames = vcat left $ text label : [vcat left (text . getName <$> inv)]
  colBal =
    vcat left
      $ text "Summa:"
      : [ vcat
            right
            (text . (\x -> show (getBalance x) <> " " <> getCurr x) <$> inv)
        ]
  colBitbar = if frmtBB
    then
      vcat right
      $ emptyBox 1 1
      : (text <$> replicate (length inv) "| href=http://fortnox.se")
    else nullBox

currentDay :: IO Day
currentDay =
  localDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)

tokenPath :: IO FilePath
tokenPath = getXdgDirectory XdgConfig $ "noxstatus" <> "/" <> "fn_token.json"

loadTokenFile :: IO FnToken
loadTokenFile = do
  tp      <- tokenPath
  cfgFile <-
    eitherDecodeFileStrict' tp
      `catch` (\e -> if isDoesNotExistError e
                then
                  die
                    "Error: Could not find token file, generate one with \"--auth\" argument"
                else die $ "Error opening token file: \n" <> show e
              )

  case cfgFile of
    Left  e -> die $ "Error parsing token file \"" <> tp <> "\":\n" <> e
    Right c -> return c

newTokenFile :: AuthCode -> IO ()
newTokenFile ac = do
  exists <- doesFileExist =<< tokenPath
  if exists
    then tokenPath >>= die . ("Token file already exists at: " <>)
    else getAccessToken ac CS.clientSecret >>= \case
      Left  e -> die $ "Error getting token file:\n" <> show e
      Right t -> do
        tp <- tokenPath
        encodeFile tp t
        putStrLn $ "Saved tokenfile to:\n" <> tp
