import qualified Data.Char as C (toLower)
import qualified Data.List as L (nub, intercalate)
import qualified Data.Map as M (Map, lookup, fromList, toList)
import Text.Regex.Posix
import Data.Maybe (fromMaybe)

type CompressionMap = M.Map String Int

buildCompressionDict :: String -> CompressionMap 
buildCompressionDict = M.fromList . flip zip [0..] . uniques 
    where uniques = L.nub . words . map C.toLower . filter (`notElem` ".,:;!?")

compressChunk :: CompressionMap -> [String] -> String
compressChunk m t = unwords . map runCompression $ t
    where runCompression s
            | s =~ "[A-Z][a-z]*"  = compressed ++ "^"
            | s =~ "[A-Z]+"       = compressed ++ "!"
            | s =~ "[a-zA-Z]+\\." = compressed ++ " " ++ "."
            | s =~ "[a-zA-Z]+\\;" = compressed ++ " " ++ ";"
            | s =~ "[a-zA-Z]+\\:" = compressed ++ " " ++ ":"
            | s =~ "[a-zA-Z]+\\," = compressed ++ " " ++ ","
            | s =~ "[a-zA-Z]+\\?" = compressed ++ " " ++ "?"
            | s =~ "[a-zA-Z]+\\!" = compressed ++ " " ++ "!"
            | otherwise           = compressed 
                where compressed    = show . fromMaybe (-1) . flip M.lookup m $ justTheWord s 
                      justTheWord s =  map C.toLower . filter (`notElem` ".,:;!?") $ s

compressText :: CompressionMap -> String -> String
compressText m t = L.intercalate " R " . map (compressChunk m . words) . lines $ t 