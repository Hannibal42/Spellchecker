module Main where
import Control.Monad 
import Data.List (foldl')
import Data.Char
import Text.Read


import System.Environment (getArgs)


data Tree a = Node a Bool [Tree a] deriving Show

main :: IO ()
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
	let words2 = words content2

	putStrLn "Generating Trie..."
	let trie = generateTree words2
	putStrLn "Trie generated!"

	putStrLn "Starting correction..." 
	let sentences1 = sentences content1
	analyseSentence trie sentences1
	print "Ende2"





analyseSentence :: [Tree Char] -> [(String,Char)] -> IO ()
analyseSentence _    []     = print "Write down the sentence" -- Den Satz aufschreiben TODO
analyseSentence trie ((cs,c):xs) = do 
    
    let words1 = words cs
    mapM_ (analyseWord trie (cs,c)) words1
    analyseSentence trie xs



analyseWord :: [Tree Char] -> (String,Char) -> String -> IO ()
analyseWord trie (cs,c) word
           | (contains trie word) = return ()
	       | otherwise            = do
	            print $ "Suggestions for " ++ word ++ " in the sentence:" 
	            print $ cs ++ [c]  

	            let resultList = (findSuggestions trie word)
	            interface resultList 10


interface :: [(String,Int)] -> Int -> IO () --Prints interface + suggestions and parses the user input
interface resultList x = do

    let modList = (modResultList x resultList)
    print $ foldl (\ string (word,index) -> string ++ word ++ " " ++ (show index) ++ ", ") "" modList
    print "Options: (i)gnore,(m)ore suggestions,pick a number"

    input <- getLine
    case input of
        "i" -> return ()
        "m" -> interface resultList (x+5)
        key -> case readMaybe key :: Maybe Int of 
            Just a -> if (a>=0) && (a>=length modList) 
                      then print "Not a valid input" >> interface resultList x
                      else print (modList !! a )
            Nothing -> print "Not a valid input" >> interface resultList x



modResultList ::  Int -> [(String,Int)] -> [(String,Int)] --adds the index and prevents out of range errors of the result list
modResultList x resultList =  if (x>len) then makeIndex [0..len] (take len resultList) else makeIndex [0..x] (take x resultList)
	                                  where
	                                 	len = length resultList
	                                 	makeIndex _     []          = []
	                                 	makeIndex (y:ys) ((a,b):xs) = (a,y) : makeIndex ys xs




{-analyseText :: [Tree Char] -> String -> IO () -- TODO: I need an interface!!!!!
analyseText ts x = if (contains ts x) 
	               then return ()
	               else print $ "Suggestions for " ++ x ++ ": " ++ (foldl (\ string (word,minEdit) -> string ++ word ++ " " ++ (show minEdit) ++ ", ") "" (take 10 (findSuggestions ts x)))
-}

-- findSuggestions trie "Halo" 
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
                                   | otherwise = let ele = min (fir+1) (min (sec+1) (c+1)) in calcRow x ys (ele:c:cr) (sec:lr)  
























quicksort :: Ord a => [(b,a)] -> [(b,a)] -- \0/ I learned this in "Grundlagen der Programmierung" TODO: Better filter function that give back a tupel ([lesser],[greater])
quicksort []            = [] 
quicksort ((word,x):xs) = (quicksort (filter (lesser x) xs) ) ++ [(word,x)] ++ (quicksort (filter (greaterEq x) xs))
                            where
                              lesser x (_,y) = x>y
                              greaterEq x (_,y) = x<=y 


















sentences :: String -> [(String,Char)]
sentences [] = []
sentences cs = go cs []
        where
        	go :: String -> String -> [(String,Char)]
        	go []         [] = []
        	go [c]        ws = if (isEnd c) then [(ws,c)] else [(ws ++ [c],' ')]
        	go (c1:c2:cs) ws
        	   | (isEnd c1) && (c2==' ') = (ws,c1) : (go cs []) 
        	   | otherwise               = go (c2:cs) (ws ++ [c1]) 

isEnd c = (c=='.') || (c=='!') || (c=='?')     -- Ending of a sentence 



















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





