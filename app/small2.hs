{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Map as M
import Data.List ( sortBy )
import Data.Function ( on )
import System.Environment ( getArgs )


-- countChars :: (Ord k) => [k] -> M.Map k Int
countChars :: String -> M.Map Char Int
countChars = M.fromListWith (+) . map (,1)

-- contains :: (Ord k) => M.Map k Int -> M.Map k Int -> Bool
contains :: M.Map Char Int -> M.Map Char Int -> Bool
a `contains` b = M.isSubmapOfBy (<=) b a

anagramPrinter :: String -> [String] -> IO ()
anagramPrinter s wordlist = combos target keys [] where
    target = countChars s
    wordmap = M.fromListWith (++) [(cx, [x]) | x <- wordlist, let cx = countChars x, target `contains` cx]
    keys = sortBy (compare `on` (negate . sum)) $ M.keys wordmap
    combos (M.null -> True) _ combo = mapM_ (putStrLn . unwords) $ mapM (wordmap M.!) combo
    combos _ [] _ = return ()
    combos t (w:ws) combo = do
        combos nt nws (w:combo) -- include 
        combos t ws combo       -- exclude
        where
            nt = M.differenceWith (\x y -> if x > y then Just (x - y) else Nothing) t w
            nws = filter (nt `contains`) (w:ws)

printHelp :: IO ()
printHelp = putStrLn "Usage: anagrams [-f <wordfile>] <word>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"]       -> printHelp
        ["-f", f, s] -> readFile f >>= anagramPrinter s . words
        [s]          -> readFile "/home/josh/.local/bin/words.txt" >>= anagramPrinter s . words
        _            -> printHelp
