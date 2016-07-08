module VersionParser
  ( parseVersion
  ) where

import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Word
import Options.Applicative (ReadM, readerError)
import Text.Parsec (many1, digit, parse, char)
import Text.Parsec.String (Parser)

decimal :: Parser Int
decimal = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

versionParser :: Parser (Word16, Word16)
versionParser = do
  major <- decimal
  char '.'
  minor <- decimal
  return (fromIntegral major, fromIntegral minor)

parseVersion :: String -> ReadM (Word16, Word16)
parseVersion xs = case parse versionParser "" xs of
                    Left _ -> readerError "bad version number"
                    Right v -> return $ v

