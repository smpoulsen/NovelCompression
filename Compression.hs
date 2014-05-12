--Need to fix hyphens. Haven't accounted for them yet.


import qualified Data.Map.Lazy as M
import Text.Regex.Posix
import Control.Applicative (liftA, liftA2, pure)
import Data.Text.Lazy (toUpper, pack, unpack)
import qualified Data.Char as C
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)

type Chunk          = String
type CompressedText = [Chunk]

main = do
    (text:_)   <- getArgs
    compressed <- readFile text
    let dict   = buildDictionary compressed
    let phrase = words . last . lines $ compressed 
    putStrLn $ fromMaybe "" $ parseChunks dict phrase

buildDictionary :: String -> M.Map Int String
buildDictionary x = M.fromList $ zip [0..] $ take (numberWords x) . tail . lines $ x
    where numberWords = read . head . lines

parseChunks :: M.Map Int String -> CompressedText -> Maybe String
parseChunks m t = liftA (unlines . lines . assembleChunks) . sequence $ decompressed
    where decompressed     = map (decompressText m) t
          assembleChunks x = foldl wordsPunctuation "" x
          wordsPunctuation acc x = if x =~ "[!,.;:\n\r]" then acc ++ x else acc ++ " " ++ x

decompressText :: M.Map Int String -> Chunk -> Maybe String
decompressText m s 
    | s =~ "[0-9]+!"   = liftA handleUpper . findValue $ s
    | s =~ "[0-9]+\\^" = liftA capitalize . findValue $ s
    | s =~ "[0-9]+"    = M.lookup (read s) m  
    | s =~ "[!?.,;:-]"  = Just s
    | s =~ "[RE]"      = Just "\n"
    | otherwise        = Nothing
        where findValue t       = M.lookup (read . init $ t) m
              handleUpper       = unpack . toUpper . pack
              capitalize (x:xs) = C.toUpper x : xs