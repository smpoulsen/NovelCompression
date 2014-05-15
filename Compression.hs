import Control.Applicative (liftA)
import qualified Data.Char as C (toLower)
import Data.Function (on)
import qualified Data.List as L (nub, intercalate, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M (Map, lookup, fromList, toList)
import Data.Maybe (fromMaybe)
import Text.Regex.Posix

type CompressionMap = M.Map String Int

main = do
    text <- getContents
    let dict = buildCompressionDict text
    putStrLn $ show . length . M.toList $ dict
    mapM_ putStrLn $ map fst . L.sortBy (compare `on` snd) . M.toList $ dict 
    putStrLn $ compressText dict text ++ " E"

buildCompressionDict :: String -> CompressionMap 
buildCompressionDict = M.fromList . flip zip [0..] . uniques 
    where uniques = L.nub . concatMap (splitOn "-") . words . map C.toLower . filter (`notElem` ".,:;!?")

compressChunk :: CompressionMap -> [String] -> String
compressChunk m t = unwords . map runCompression $ t
    where runCompression s
            | s =~ "[a-zA-Z]+\\-[a-zA-Z]+" = fromMaybe "-1" $ handleHyphens s 
            | s =~ "[A-Z][a-z]*"  = compressed ++ "^"
            | s =~ "[A-Z]+"       = compressed ++ "!"
            | s =~ "[a-zA-Z]+\\." = compressed ++ " " ++ "."
            | s =~ "[a-zA-Z]+\\;" = compressed ++ " " ++ ";"
            | s =~ "[a-zA-Z]+\\:" = compressed ++ " " ++ ":"
            | s =~ "[a-zA-Z]+\\," = compressed ++ " " ++ ","
            | s =~ "[a-zA-Z]+\\?" = compressed ++ " " ++ "?"
            | s =~ "[a-zA-Z]+\\!" = compressed ++ " " ++ "!"
            | otherwise           = compressed 
                where compressed      = show . fromMaybe (-1) . flip M.lookup m $ justTheWord s 
                      justTheWord t   = map C.toLower . filter (`notElem` ".,:;!?") $ t
                      handleHyphens t = liftA (L.intercalate " - " . map show) . sequence . map (flip M.lookup m . justTheWord) . splitOn "-" $ t

compressText :: CompressionMap -> String -> String
compressText m t = L.intercalate " R " . map (compressChunk m . words) . lines $ t 