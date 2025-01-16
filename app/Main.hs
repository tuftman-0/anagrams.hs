{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Main where
import qualified Data.DList as D
import qualified Data.Map as M
-- import Data.List ( intercalate, sortBy )
import Data.List 
import Data.Function (on)
import System.Environment (getArgs)
import Data.Char ( toLower, isLower )
import Control.Parallel
-- import Control.Parallel.Strategies
-- import Control.Applicative
-- import qualified Control.Monad.Par.IO as P
import qualified Data.Set as Set
import Data.Maybe

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


-- dropwhile :: (a -> Bool) -> [a] -> [a]
-- dropwhile _ [] = []
-- dropwhile p (x:xs)
--     | p x       = dropwhile p xs
--     | otherwise = x:xs

-- dropwhile' :: (a -> Bool) -> [a] -> [a]
-- dropwhile' p l = foldr f l l where
--     f x a | p x       = tail a
--           | otherwise = l


-- nubOrd :: (Ord a) => [a] -> [a]
-- nubOrd = go Set.empty where
--   go s (x:xs)
--    | x `Set.member` s = go s xs
--    | otherwise        = x : go (Set.insert x s) xs
--   go _ _              = []


-- subtractMap :: (Ord k) => M.Map k Int -> M.Map k Int -> M.Map k Int
-- subLetter :: CharCombo -> CharCombo -> CharCombo
-- sub1 :: Char -> CharCombo -> CharCombo
-- sub1 = M.update f where
--     f x | x > 1     = Just (x - 1)
--         | otherwise = Nothing


-- insert' :: Ord a => a -> [a] -> [a]
-- insert' y [] = [y]
-- insert' y (x:xs)
--     | y < x     = x:insert y xs
--     | otherwise = y:x:xs

-- insert :: Ord a => a -> [a] -> [a]
-- insert x = foldr f [x] where
--     f y acc | x < y     = y : acc
--             | otherwise = x : y : acc

-- insort :: Ord a => [a] -> [a]
-- insort = foldr insert' []

type CharCombo = M.Map Char Int

fixWord :: String -> String
fixWord = filter isLower . map toLower


countChars :: String -> CharCombo
countChars = M.fromListWith (+) . map (,1) . fixWord

contains :: CharCombo -> CharCombo -> Bool
a `contains` b = M.isSubmapOfBy (<=) b a
-- a `contains` b = M.foldrWithKey f True b where
--     f k v acc = acc && case M.lookup k a of
--         Just v' -> v' >= v
--         _       -> False

-- subtractMap :: (Ord k) => M.Map k Int -> M.Map k Int -> M.Map k Int
subtractMap :: CharCombo -> CharCombo -> CharCombo
subtractMap = M.differenceWith f where
    f x y | x > y     = Just (x - y)
          | otherwise = Nothing


sortByLen :: [CharCombo] -> [CharCombo]
sortByLen = sortBy (compare `on` (negate . sum))

-- this produces a difference list of all of the valid combinations of
-- CharCombos in the wordlist that combine to make a target CharCombo
comboCombos :: CharCombo -> [CharCombo] -> [CharCombo] -> D.DList [CharCombo]
comboCombos (M.null -> True) _ combo = D.singleton $ reverse combo
comboCombos _ [] _= D.empty
comboCombos target (w:ws) combo = comboCombos ntarget nws (w:combo) <> comboCombos target ws combo where
    ntarget = target `subtractMap` w
    nws = filter (ntarget `contains`) (w:ws)

-- this produces a list of all the anagrams of a word using a wordlist
anagrams :: String -> [String] -> [[String]]
anagrams word wordlist = concatMap (mapM (wordmap M.!)) combos where
    target = countChars word
    wordmap = M.fromListWith (++) [(cx, [x]) | x <- wordlist
                                   ,let cx = countChars x
                                   ,target `contains` cx]
    keys = sortByLen $ M.keys wordmap
    combos = comboCombos target keys []


-- this does the same thing as comboCombos but in parallel
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


-- parAnagrams word wordlist = concat $ fmap (traverse (wordmap M.!)) $ parComboCombos target keys [] where
-- this produces a list of all the anagrams of a word using a wordlist
parAnagrams :: String -> [String] -> [[String]]
-- parAnagrams word wordlist =  concat $ parMap rpar (traverse (wordmap M.!)) combos where
parAnagrams word wordlist = concatMap (traverse (wordmap M.!)) combos where -- `using` parListChunk 8 rdeepseq where
    target = countChars word
    wordmap = M.fromListWith (++) [(cx, [x]) | x <- wordlist
                                   ,let cx = countChars x
                                   ,target `contains` cx]
    keys = sortByLen $ M.keys wordmap
    -- combos = D.toList $ parComboCombos target keys []
    combos = parComboCombos target keys []


-- this does the same thing as anagrams, except it just prints them because it's faster
anagramPrinter :: String -> [String] -> IO ()
anagramPrinter s wordlist = mf target keys [] where
    target = countChars s
    wordmap = M.fromListWith (++) [(cx, [x]) | x <- wordlist ,let cx = countChars x ,target `contains` cx]
    keys = sortByLen $ M.keys wordmap
    mf (M.null -> True) _ combo = mapM_ (putStrLn . unwords) $ mapM (wordmap M.!) $ reverse combo
    mf _ [] _ = return ()
    mf t (w:ws) combo = do
        mf nt nps (w:combo)
        mf t ws combo
        where
            nt = t `subtractMap` w
            nps = filter (nt `contains`) (w : ws)

printAnagrams :: FilePath -> String -> IO ()
printAnagrams file word = readFile file >>= anagramPrinter word . words

printAnagramsInline :: FilePath -> String -> IO ()
printAnagramsInline file word = readFile file >>= putStr . unwords . intercalate ["|"] . anagrams word . words

printAnagramsParallel :: FilePath -> String -> IO ()
printAnagramsParallel file word = readFile file >>= mapM_ (putStrLn . unwords) . parAnagrams word . words

text :: IO [String]
text = words <$> readFile defaultFile

defaultFile :: String
defaultFile = "/home/josh/.local/bin/words.txt"

printHelp :: IO ()
printHelp = putStrLn "Usage: anagrams [-f <wordfile>] <word>"

main :: IO ()
main = do
    args <- getArgs
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
