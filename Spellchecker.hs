module Main where
import Control.Monad 
import Data.List (foldl')


import System.Environment (getArgs)


data Tree a = Node a Bool [Tree a] deriving Show


main :: IO()
main = do
	args <- getArgs
	case args of
		[file1,file2] -> mainSpellcheck file1 file2 
		_ -> putStrLn "Expecting exactly two filenames as command line arguments."

mainSpellcheck :: FilePath -> FilePath -> IO ()
mainSpellcheck file1 file2 = do 
	putStrLn "Reading Files..."
	content1 <- readFile file1
	content2 <- readFile file2
	let words1 = words content1
	let words2 = words content2
	putStrLn "Generating Trie..."
	let trie = generateTree words2
	putStrLn "Tree generated!" 
	mapM_ (analyseText trie) words1



-- findSuggestions trie "Halo" 

analyseText :: [Tree Char] -> String -> IO () -- TODO: I need an interface!!!!!
analyseText ts x = if (contains ts x) 
	               then return ()
	               else print $ "Suggestions for " ++ x ++ ": " ++ (foldl (\ string (word,minEdit) -> string ++ word ++ " " ++ (show minEdit) ++ ", ") "" (take 10  findSuggestions ts x))


findSuggestions :: [Tree Char] -> String -> [(String,Int)]
findSuggestions ts word = quicksort $ calcSuggestions ts [] word 8 1 [0..(length word)]

{- calcSuggestions trie [] "Halo" 8 1 [0..(length "Halo")]
Parameters: Trie , the tempWord we build up while traversing the trie, the word we want suggestions for, the treshold for the minimum Editdistance, a accu, the row from the rood node-}
calcSuggestions :: [Tree Char] -> String -> String -> Int -> Int -> [Int] -> [(String,Int)] -- TODO: Build a function that gives back True if the minEdit distance still can become better than the treshold, TODO: Delete the accu
calcSuggestions []                _        _    _      _   _     = []
calcSuggestions ((Node a b c):ts) tempWord word thresh acc lastrow
               | b && (minEdit < thresh) =   -- word end and minimum Editdistance smaller than the threshold,
                          (reverse (a:tempWord),minEdit) -- adding a new tupel to the result list,
                          : (calcSuggestions ts tempWord word thresh acc lastrow) --searching through the remaining nodes on the same level,
                          ++ (calcSuggestions c (a:tempWord) word thresh (acc+1) currow)  -- searching through the child nodes.
               | otherwise = 
                          (calcSuggestions ts tempWord word thresh acc lastrow) --searching through the remaining nodes on the same level,
                          ++ (calcSuggestions c (a:tempWord) word thresh (acc+1) currow) -- searching through the child nodes.
                            where
                              row     = (calcRow a word [acc] lastrow)
                              currow  = reverse row
                              minEdit = head row

calcRow ::  Char -> [Char] -> [Int] -> [Int] -> [Int] --TODO: Build a function that calculates the cost with the confusion matrix
calcRow _ []     cr     lastrow      = cr
calcRow x (y:ys) (c:cr) (fir:sec:lr)
                                   | (x==y)    = let ele = min fir (min (sec+1) (c+1)) in calcRow x ys (ele:c:cr) (sec:lr)
                                   | otherwise = let ele = min (fir+2) (min (sec+1) (c+1)) in calcRow x ys (ele:c:cr) (sec:lr)  


quicksort :: Ord a => [(b,a)] -> [(b,a)] -- \0/ I learned this in "Grundlagen der Programmierung" TODO: Better filter function that give back a tupel ([lesser],[greater])
quicksort []            = [] 
quicksort ((word,x):xs) = (quicksort (filter (lesser x) xs) ) ++ [(word,x)] ++ (quicksort (filter (greaterEq x) xs))
                            where
                              lesser x (_,y) = x>y
                              greaterEq x (_,y) = x<=y 





generateTree :: [String] -> [Tree Char]
generateTree ws = foldl' (insertInTree) [] ws

insertInTree :: [Tree Char] -> String -> [Tree Char] -- main function to generate the trie 
insertInTree []                [x]     = [(Node x True [])]   
insertInTree []                (x:xs)  = [(Node x False (insertInTree [] xs))]  
insertInTree ((Node a b c):ts) [x]     = if x == a 
	                                    then (Node a True c):ts 
	                                    else (Node a b c):(insertInTree ts [x])
insertInTree ((Node a b c):ts) (x:xs)  = if x == a 
	                                    then (Node a b (insertInTree c xs)):ts 
	                                    else (Node a b c):(insertInTree ts (x:xs))

contains :: [Tree Char] -> String -> Bool   -- checks if a word is in a given trie 
contains _  [] = True
contains [] _  = False
contains ((Node a b c):ts) [x]    = if a == x
	                                then b
	                                else contains ts [x]
contains ((Node a b c):ts) (x:xs) = if a == x
	                                then contains c  xs
	                                else contains ts (x:xs)


-- Some Test for tree testing
treeSize :: [Tree Char] -> Integer -- Returns the number of nodes in the tree
treeSize []                = 0
treeSize ((Node a b c):ts) = (treeSize c) + (treeSize ts) + 1


treeTest:: [String] -> [Tree Char] -> Bool   -- A short function to verify that all words that are in the wordlist are in the trie
treeTest []     _  = True
treeTest (x:xs) ts = (contains ts x) && (treeTest xs ts) 








diff :: Eq a => [a] -> [a] ->  Int --My diff from the previous task for testing, also I reused my calcRow function
diff xs ys = (matrix !! 0) !! 0
            where
               matrix = map reverse $ calcMatrix xs ys 1 [[0..(length ys)]] 

--calcMatrix "Hallo" "Hi" 1 [[0..(length "Hi")]]

calcMatrix :: Eq a => [a] -> [a] -> Int -> [[Int]] -> [[Int]]
calcMatrix []     _  acc ms     = ms 
calcMatrix (x:xs) ys acc (m:ms) = calcMatrix xs ys (acc+1) ((calcRow2 x ys [acc] m):m:ms)
                                 	

calcRow2 :: Eq a => a -> [a] -> [Int] -> [Int] -> [Int]
calcRow2 _ []     rs     ts       = reverse rs
calcRow2 x (y:ys) (r:rs) (a:b:ts)
                                | (x == y)  = let ele = min a (min (b+1) (r+1)) in calcRow2 x ys (ele:r:rs) (b:ts)
                                | otherwise = let ele = min (a+2) (min (b+1) (r+1)) in calcRow2 x ys (ele:r:rs) (b:ts)