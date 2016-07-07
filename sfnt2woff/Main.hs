module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Graphics.WOFF
import Options.Applicative
import System.Exit
import System.FilePath

data CommandOption = CommandOption
  { otffile :: String
  } deriving (Show)

opts = info (helper <*> cmdOpt)
  ( fullDesc
  <> progDesc "package OpenType <otffile> as WOFF, creating <otffile>.woff")
  where
    cmdOpt = CommandOption <$> argument str (metavar "otffile")

majorVersion :: Word16
majorVersion = 0

minorVersion :: Word16
minorVersion = 0

main :: IO ()
main = do
  cmd <- execParser opts
  let infile = otffile cmd
  sfnt <- BS.readFile infile
  result <- encode sfnt majorVersion minorVersion
  case result of
    Left errorCode -> do
      print errorCode -- FIXME: pretty print error code
      exitWith (ExitFailure 1)
    Right (woff, warnings) -> do
      let outfile = replaceExtension infile ".woff"
      BS.writeFile outfile woff

