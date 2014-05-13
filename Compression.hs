import qualified Data.Char as C
import qualified Data.Map.Lazy as M
import Control.Applicative (liftA)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (toUpper, replace, pack, unpack)
import System.Environment (getArgs)
import Text.Regex.Posix

type Chunk          = String
type CompressedText = [Chunk]
type CompressionMap = M.Map Int String

main = do
    (text:_)   <- getArgs
    compressed <- readFile text
    putStr $ fromMaybe "Error: Input resulted in a nothing.\n" $ fixHyphens . decompressText . parseInput $ compressed

fixHyphens :: Maybe String -> Maybe String
fixHyphens x = liftA (unpack . replace (pack "- ") (pack "-")) $ liftA pack x

parseInput :: String -> (CompressionMap, CompressedText)
parseInput x = (dict x, phrase x)
    where splitInput x = splitAt (read . head . lines $ x) . tail . lines $ x
          dict         = M.fromList . zip [0..] . fst . splitInput
          phrase       = words . concat . snd . splitInput

decompressText :: (CompressionMap, CompressedText) -> Maybe String
decompressText (m, t) = liftA assembleChunks . sequence $ decompressed
    where decompressed     = map (parseChunks m) t
          assembleChunks   = foldl wordsPunctuation ""
          wordsPunctuation acc x 
            | x =~ "[!,.;:\n\r-]" = acc ++ x
            | otherwise           = acc ++ " " ++ x

parseChunks :: CompressionMap -> Chunk -> Maybe String
parseChunks m s 
    | s =~ "[0-9]+!"   = liftA (map C.toUpper) . findValue $ s
    | s =~ "[0-9]+\\^" = liftA capitalize . findValue $ s
    | s =~ "[0-9]+"    = M.lookup (read s) m  
    | s =~ "[!?.,;:-]" = Just s
    | s =~ "[RE]"      = Just "\n"
    | otherwise        = Nothing
        where findValue t       = M.lookup (read . init $ t) m
              capitalize (x:xs) = C.toUpper x : xs