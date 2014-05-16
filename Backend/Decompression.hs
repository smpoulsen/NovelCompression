module Backend.Decompression
  ( runDecompression
  ) where

import qualified Data.Char as C
import qualified Data.Map.Lazy as M
import Control.Applicative (liftA)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (toUpper, replace, pack, unpack)
import Text.Regex.Posix

type Chunk          = String
type CompressedText = [Chunk]
type DecompressionMap = M.Map Int String
type CompressionMap = M.Map String Int

runDecompression :: String -> String
runDecompression s = fromMaybe "Error: Input resulted in a nothing.\n" $ fixHyphens . decompressText . parseCompressedInput $ s

fixHyphens :: Maybe String -> Maybe String
fixHyphens x = liftA (unpack . replace (pack "- ") (pack "-")) $ liftA pack x

parseCompressedInput :: String -> (DecompressionMap, CompressedText)
parseCompressedInput x = (dict x, phrase x)
    where splitInput x = splitAt (read . head . lines $ x) . tail . lines $ x
          dict         = M.fromList . zip [0..] . fst . splitInput
          phrase       = words . concat . snd . splitInput

decompressText :: (DecompressionMap, CompressedText) -> Maybe String
decompressText (m, t) = liftA assembleChunks . sequence $ decompressed
    where decompressed     = map (parseChunks m) t
          assembleChunks   = foldl wordsPunctuation ""
          wordsPunctuation acc x 
            | acc == ""              = acc ++ x
            | x =~ "[\\?!,.;:\n\r-]" = acc ++ x
            | last acc == '\n'       = acc ++ x          
            | otherwise              = acc ++ " " ++ x

parseChunks :: DecompressionMap -> Chunk -> Maybe String
parseChunks m s 
    | s =~ "[0-9]+!"   = liftA (map C.toUpper) . findValue $ s
    | s =~ "[0-9]+\\^" = liftA capitalize . findValue $ s
    | s =~ "[0-9]+"    = M.lookup (read s) m  
    | s =~ "[!?.,;:-]" = Just s
    | s =~ "[RE]"      = Just "\n"
    | otherwise        = Nothing
        where findValue t       = M.lookup (read . init $ t) m
              capitalize (x:xs) = C.toUpper x : xs