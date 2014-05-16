import System.Environment (getArgs)
import Backend.Compression
import Backend.Decompression

main = do
    (flag:fIn:fOut:_) <- getArgs
    text              <- readFile fIn
    case flag of
        "-c" -> compress fOut text
        "-d" -> decompress fOut text
        otherwise -> putStrLn "Invalid flag.\nUse: -c = compression; -d = decompression."

compress :: FilePath -> String -> IO () 
compress x s = writeFile x $ runCompression s

decompress :: FilePath -> String -> IO ()
decompress x s = writeFile x $ runDecompression s