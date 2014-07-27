data Stats = Stats { count :: Integer
                   , longest :: String }
                   deriving (Show)

lineStats :: IO Stats
lineStats = runStats $ Stats { count = 0, longest = "" }

runStats :: Stats -> IO Stats
runStats stats = do
    line <- getLine
    if line == ""
        then return stats
        else runStats $ updateStats line stats

updateStats :: String -> Stats -> Stats
updateStats line stats =
    Stats { count = newCount, longest = newLongest }
        where newCount   = 1 + count stats
              newLongest = 
                if length line > (length $ longest stats)
                    then line
                    else longest stats
