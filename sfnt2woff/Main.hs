module Main where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Word
import Data.IORef
import Graphics.WOFF
import Options.Applicative
import System.Exit
import System.FilePath

import VersionParser

data CommandOption = CommandOption
  { version :: Maybe (Word16, Word16)
  , metadataFile :: Maybe String
  , privateDataFile :: Maybe String
  , otfFile :: String
  } deriving (Show)

opts :: ParserInfo CommandOption
opts = info (helper <*> cmdOpt)
  ( fullDesc
  <> progDesc "package OpenType <otffile> as WOFF, creating <otffile>.woff")
  where
    cmdOpt = CommandOption
      <$> optional (option (str >>= parseVersion)
          ( short 'v'
          <> metavar "<maj>.<min>"
          <> help "set font version number (major and minor, both integers)"))
      <*> optional (strOption
          ( short 'm'
          <> metavar "metadata.xml"
          <> help "include metadata from <metadata.xml> (not validated)"))
      <*> optional (strOption
          ( short 'p'
          <> metavar "private.data"
          <> help "include private data block"))
      <*> argument str (metavar "otffile")

defaultVersion :: (Word16, Word16)
defaultVersion = (0, 0)

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) s = s x
whenJust Nothing _ = pure ()

main :: IO ()
main = do
  woffRef <- newIORef (BS.empty)
  cmd <- execParser opts
  let infile = otfFile cmd
  sfnt <- BS.readFile infile
  let (major, minor) = fromMaybe defaultVersion (version cmd)
  result <- encode sfnt major minor
  checkResult result woffRef

  whenJust (metadataFile cmd) (\f -> do
    meta <- BS.readFile f
    woff <- readIORef woffRef
    result' <- setMetadata woff meta
    checkResult result' woffRef)

  whenJust (privateDataFile cmd) (\f -> do
    priv <- BS.readFile f
    woff <- readIORef woffRef
    result' <- setPrivateData woff priv
    checkResult result' woffRef)

  let outfile = replaceExtension infile ".woff"
  woff <- readIORef woffRef
  BS.writeFile outfile woff

  where
    checkResult result woffRef =
      case result of
        Left errorCode -> do
          print errorCode
          exitWith (ExitFailure 1)
        Right (woff, _) -> do
          writeIORef woffRef woff

