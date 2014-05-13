import qualified Data.Char as C
import qualified Data.Map.Lazy as M
import Control.Applicative (liftA)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (toUpper, replace, pack, unpack)
import System.Environment (getArgs)
import Text.Regex.Posix

type Chunk          = String
type CompressedText = [Chunk]

main = do
    (text:_)   <- getArgs
    compressed <- readFile text
    let parsedInput = parseInput compressed
    putStr $ fromMaybe "Error: Input resulted in a nothing.\n" $ fixHyphens . decompressText $ parsedInput

fixHyphens :: Maybe String -> Maybe String
fixHyphens x = liftA (unpack . replace (pack "- ") (pack "-")) $ liftA pack x

parseInput :: String -> (M.Map Int String, CompressedText)
parseInput x = (dict x, phrase x)
    where splitInput x = splitAt (read . head . lines $ x) . tail . lines $ x
          dict         = M.fromList . zip [0..] . fst . splitInput
          phrase       = words . concat . snd . splitInput

decompressText :: (M.Map Int String, CompressedText) -> Maybe String
decompressText (m, t) = liftA assembleChunks . sequence $ decompressed
    where decompressed     = map (parseChunks m) t
          assembleChunks   = foldl wordsPunctuation ""
          wordsPunctuation acc x 
            | x =~ "[!,.;:\n\r-]" = acc ++ x
            | otherwise           = acc ++ " " ++ x

parseChunks :: M.Map Int String -> Chunk -> Maybe String
parseChunks m s 
    | s =~ "[0-9]+!"   = liftA handleUpper . findValue $ s
    | s =~ "[0-9]+\\^" = liftA capitalize . findValue $ s
    | s =~ "[0-9]+"    = M.lookup (read s) m  
    | s =~ "[!?.,;:-]" = Just s
    | s =~ "[RE]"      = Just "\n"
    | otherwise        = Nothing
        where findValue t       = M.lookup (read . init $ t) m
              handleUpper       = unpack . toUpper . pack
              capitalize (x:xs) = C.toUpper x : xs