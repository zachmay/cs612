import Control.Applicative
import System.Directory

process :: String -> String
process = unlines
        . map unwords
        . map (map reverse)
        . map words
        . lines

reverseWords :: IO ()
reverseWords = do
    putStr "Enter a file name: "
    fileName <- getLine
    fileExists <- doesFileExist fileName
    if fileExists then do
        process <$> readFile fileName >>= putStr
    else do
        putStrLn "File does not exist. Try again."
        reverseWords

main :: IO ()
main = reverseWords
