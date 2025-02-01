{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Main where
import qualified Data.DList as D
import qualified Data.Map as M
import Data.List ( intercalate, sortBy )
import Data.Char ( toLower, isLower )
import Data.Function (on)
import System.Environment (getArgs)
import System.Directory ( getHomeDirectory )
import Control.Parallel ( par, pseq )

{-
 - basically what this program does is take in a word list representing all of
 - the valid permutations of characters (words), and reduces it to the set of
 - all valid combinations of characters (CharCombos) and groups all words with
 - the same CharCombo. It then finds all of the combinations of CharCombos that
 - "sum" to make a target word.
 - the combination of letters in the combination of CharCombos will exactly equal
 - the combination of letters in the  target word
 -
 - To get the list of word combinations from the list of CharCombo combinations,
 - for each combination, we do a cartesian product over the list of words that
 - share the same CharCombo for each CharCombo in the combination
-}


type CharCombo = M.Map Char Int

fixWord :: String -> String
fixWord = filter isLower . map toLower


countChars :: String -> CharCombo
countChars = M.fromListWith (+) . map (,1) . fixWord

contains :: CharCombo -> CharCombo -> Bool
a `contains` b = M.isSubmapOfBy (<=) b a

-- subtractMap :: (Ord k) => M.Map k Int -> M.Map k Int -> M.Map k Int
subtractMap :: CharCombo -> CharCombo -> CharCombo
subtractMap = M.differenceWith f where
    f x y | x > y     = Just (x - y)
          | otherwise = Nothing


sortByLen :: [CharCombo] -> [CharCombo]
sortByLen = sortBy (compare `on` (negate . sum))

-- this produces a difference list of all of the valid combinations of CharCombos in the wordlist that combine to make the target
comboCombos :: CharCombo -> [CharCombo] -> [CharCombo] -> D.DList [CharCombo]
comboCombos (M.null -> True) _ combo = D.singleton $ reverse combo
comboCombos _ [] _= D.empty
comboCombos target (w:ws) combo = comboCombos ntarget nws (w:combo) <> comboCombos target ws combo where
    ntarget = target `subtractMap` w
    nws = filter (ntarget `contains`) (w:ws)

-- this produces a list of all the extended anagrams of a word using a wordlist
anagrams :: String -> [String] -> [[String]]
anagrams word wordlist = concatMap (mapM (wordmap M.!)) combos where
    target = countChars word
    wordmap = M.fromListWith (++) [(cx, [x]) | x <- wordlist
                                   ,let cx = countChars x
                                   ,target `contains` cx]
    keys = sortByLen $ M.keys wordmap
    combos = comboCombos target keys []


-- this does the same thing as comboCombos but in parallel
-- running in parallel does not increase performance much while using a lot more resources
parComboCombos :: CharCombo -> [CharCombo] -> [CharCombo] -> D.DList [CharCombo]
parComboCombos (M.null -> True) _ combo = D.singleton $ reverse combo
parComboCombos _ [] _= D.empty
parComboCombos target (w:ws) combo = include `par` exclude `pseq` (include <> exclude) where
    ntarget = target `subtractMap` w
    -- filt = filter (ntarget `contains`) (w:ws) 
    -- nws = exclude `par` filt `pseq` filt
    nws = filter (ntarget `contains`) (w:ws)
    include = parComboCombos ntarget nws (w:combo)
    exclude = parComboCombos target ws combo


-- does the same thing as anagrams but in parallel
parAnagrams :: String -> [String] -> [[String]]
-- testing various versions where parAnagrams is also parallel. none really ended up gaining much (if any) performance
-- parAnagrams word wordlist = concat $ fmap (traverse (wordmap M.!)) $ parComboCombos target keys [] where
-- parAnagrams word wordlist =  concat $ parMap rpar (traverse (wordmap M.!)) combos where
parAnagrams word wordlist = concatMap (traverse (wordmap M.!)) combos where -- `using` parListChunk 8 rdeepseq where
    target = countChars word
    wordmap = M.fromListWith (++) [(cx, [x]) | x <- wordlist
                                   ,let cx = countChars x
                                   ,target `contains` cx]
    keys = sortByLen $ M.keys wordmap
    -- combos = D.toList $ parComboCombos target keys []
    combos = parComboCombos target keys []


-- this does the same thing as anagrams, except it just prints them instead of making a list because it's faster
anagramPrinter :: String -> [String] -> IO ()
anagramPrinter s wordlist = combos target keys [] where
    target = countChars s
    wordmap = M.fromListWith (++) [(cx, [x]) | x <- wordlist ,let cx = countChars x ,target `contains` cx]
    keys = sortByLen $ M.keys wordmap
    combos (M.null -> True) _ combo = mapM_ (putStrLn . unwords) $ mapM (wordmap M.!) $ reverse combo
    combos _ [] _ = return ()
    combos t (w:ws) combo = do
        combos nt nps (w:combo)
        combos t ws combo
        where
            nt = t `subtractMap` w
            nps = filter (nt `contains`) (w : ws)

printAnagrams :: FilePath -> String -> IO ()
printAnagrams file word = readFile file >>= anagramPrinter word . words

printAnagramsInline :: FilePath -> String -> IO ()
printAnagramsInline file word = readFile file >>= putStr . unwords . intercalate ["|"] . anagrams word . words

printAnagramsParallel :: FilePath -> String -> IO ()
printAnagramsParallel file word = readFile file >>= mapM_ (putStrLn . unwords) . parAnagrams word . words

-- text :: IO [String]
-- text = words <$> readFile defaultFile

defaultPath :: String
defaultPath = ".local/bin/words.txt"


-- printHelp :: IO ()
-- printHelp = putStrLn "Usage: anagrams [-f <wordfile>] <word>"

printHelp :: IO ()
printHelp = mapM_ putStrLn
    [ "Anagram Finder - finds extended anagrams for a given input"
    , ""
    , "Usage:"
    , "  anagrams [OPTIONS] [WORD]"
    , ""
    , "Options:"
    , "  -h           Show this help message"
    , "  -f FILE      Use custom dictionary file (default: ~/.local/bin/words.txt)"
    , "  -l           Print results inline with '|' separator"
    , "  -p           Use parallel processing for computation"
    , ""
    , "Examples:"
    , "  anagrams listen              # Find anagrams of 'listen' using default dictionary"
    , "  anagrams -f mydict.txt stop  # Find anagrams of 'stop' using custom dictionary"
    , "  anagrams -l silent           # Find anagrams of 'silent' and print inline"
    , "  echo \"stop\" | anagrams     # Read word from standard input"
    , ""
    , "Notes:"
    , "  - Without a WORD argument, the program reads from standard input"
    , "  - The -l option is useful for parsing output in scripts"
    , "  - The -p option may improve performance on larger words but will probably just use a lot more memory"
    ]

main :: IO ()
main = do
    home <- getHomeDirectory
    args <- getArgs
    let defaultFile = home ++ "/" ++ defaultPath
    case args of
        ["-h"]       -> printHelp
        ["-f", f, s] -> printAnagrams f s
        ["-f", f]    -> getContents >>= printAnagrams f
        ["-l"]       -> getContents >>= printAnagramsInline defaultFile
        ["-p"]       -> getContents >>= printAnagramsParallel defaultFile
        []           -> getContents >>= printAnagrams defaultFile
        ["-l", s]    -> printAnagramsInline defaultFile s
        ["-p", s]    -> printAnagramsParallel defaultFile s
        [s]          -> printAnagrams defaultFile s
        _            -> printHelp

