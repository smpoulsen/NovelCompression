module Backend.Compression 
    ( runCompression    
    ) where

import Control.Applicative (liftA)
import qualified Data.Char as C (toLower)
import Data.Function (on)
import qualified Data.List as L (nub, intercalate, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M (Map, lookup, fromList, toList)
import Data.Maybe (fromMaybe)
import Text.Regex.Posix

type CompressionMap = M.Map String Int


runCompression :: String -> String
runCompression s = dictLength ++ "\n" ++ unlines wordDict ++ compressed 
    where dict       = buildCompressionDict s 
          dictLength = show . length . M.toList $ dict
          wordDict   = map fst . L.sortBy (compare `on` snd) . M.toList $ dict 
          compressed = compressText dict s ++ " E"

buildCompressionDict :: String -> CompressionMap 
buildCompressionDict = M.fromList . flip zip [0..] . uniques 
    where uniques = L.nub . concatMap (splitOn "-") . words . map C.toLower . filter (`notElem` ".,:;!?")

compressChunk :: CompressionMap -> [String] -> String
compressChunk m t = unwords . map findCase $ t
    where findCase s
            | s =~ "[a-zA-Z]+\\-[a-zA-Z]+" = handleHyphens s 
            | s =~ "[A-Z]+[^a-z]" = compressed s ++ "!" ++ findPunctuation s 
            | s =~ "[A-Z][a-z]*"  = compressed s ++ "^" ++ findPunctuation s
            | otherwise           = compressed s ++ findPunctuation s
          findPunctuation s
            | s =~ "[a-zA-Z]+\\." = " " ++ "."
            | s =~ "[a-zA-Z]+\\;" = " " ++ ";"
            | s =~ "[a-zA-Z]+\\:" = " " ++ ":"
            | s =~ "[a-zA-Z]+\\," = " " ++ ","
            | s =~ "[a-zA-Z]+\\?" = " " ++ "?"
            | s =~ "[a-zA-Z]+\\!" = " " ++ "!"
            | otherwise           = " "
          compressed      = show . fromMaybe (-1) . flip M.lookup m . justTheWord  
          justTheWord t   = map C.toLower . filter (`notElem` ".,:;!?") $ t
          handleHyphens t = L.intercalate " - " . map findCase . splitOn "-" $ t

compressText :: CompressionMap -> String -> String
compressText m t = L.intercalate " R " . map (compressChunk m . words) . lines $ t 