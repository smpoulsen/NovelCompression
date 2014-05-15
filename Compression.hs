import qualified Data.Char as C (toLower)
import qualified Data.List as L (nub, intercalate)
import qualified Data.Map as M (Map, lookup, fromList, toList)
import Text.Regex.Posix
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

type CompressionMap = M.Map String Int

buildCompressionDict :: String -> CompressionMap 
buildCompressionDict = M.fromList . flip zip [0..] . uniques 
    where uniques = L.nub . words . map C.toLower . filter (`notElem` ".,:;!?")

compressChunk :: CompressionMap -> [String] -> String
compressChunk m t = unwords . map runCompression $ t
    where runCompression s
            | s =~ "[A-Z][a-z]*"  = compressed s ++ "^"
            | s =~ "[A-Z]+"       = compressed s ++ "!"
            | s =~ "[a-zA-Z]+\\." = compressed s ++ " " ++ "."
            | s =~ "[a-zA-Z]+\\;" = compressed s ++ " " ++ ";"
            | s =~ "[a-zA-Z]+\\:" = compressed s ++ " " ++ ":"
            | s =~ "[a-zA-Z]+\\," = compressed s ++ " " ++ ","
            | s =~ "[a-zA-Z]+\\?" = compressed s ++ " " ++ "?"
            | s =~ "[a-zA-Z]+\\!" = compressed s ++ " " ++ "!"
            | s =~ "[a-zA-Z]+-[a-zA-Z]+" = hyphenHandler $ (\([x, y]) -> (x,y)) . splitOn "-" $ s
            | otherwise           = compressed 
                where compressed  = show . fromMaybe (-1) . flip M.lookup m . justTheWord 
                      justTheWord = map C.toLower . filter (`notElem` ".,:;!?") 
                      hyphenHandler x = (compressed . fst $ x) ++ " - " ++ (compressed . snd $ x)

compressText :: CompressionMap -> String -> String
compressText m t = L.intercalate " R " . map (compressChunk m . words) . lines $ t 