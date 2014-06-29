module Main where

import Data.List (foldl')
import Text.Read (readMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
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
    print "Ende"

analyseSentence :: [Tree Char] -> [String] -> IO ()
analyseSentence _    []     = print "Write down the sentence" -- Den Satz aufschreiben TODO
analyseSentence trie (cs:xs) = do 
    
    let words1 = words cs
    mapM_ (analyseWord trie cs) words1
    analyseSentence trie xs

analyseWord :: [Tree Char] -> String -> String -> IO ()
analyseWord trie cs word
           | (contains trie word) = return ()
	       | otherwise            = do
	            print $ "Suggestions for " ++ word ++ " in the sentence:" 
	            print $ cs 

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




















-- findSuggestions trie "Halo" 
findSuggestions :: [Tree Char] -> String -> [(String,Int)]
findSuggestions ts word = sortBy (comparing snd) (calcSuggestions ts [] word 8 1 [0..(length word)])



{- calcSuggestions trie [] "Halo" 8 1 [0..(length "Halo")]
Parameters: Trie , the tempWord we build up while traversing the trie, the word we want suggestions for, the treshold for the minimum Editdistance, a accu, the row from the rood node-}
calcSuggestions :: [Tree Char] -> String -> String -> Int -> Int -> [Int] -> [(String,Int)] 
calcSuggestions []                _        _    _      _   _ = []
calcSuggestions ((Node a b c):ts) tempWord word thresh acc lastrow
               | b  && (bestEdit < thresh)  && (editDis < thresh)=                                              -- word end and minimum Editdistance smaller than the threshold,
                             (reverse (a:tempWord),editDis)                                -- adding a new tupel to the result list,
                          :  (calcSuggestions ts tempWord word thresh acc lastrow)         -- searching through the remaining nodes on the same level,
                          ++ (calcSuggestions c (a:tempWord) word thresh (acc+1) currow)   -- searching through the child nodes.
            
               | b  && (editDis < thresh) && (editDis < thresh) = 
                            (reverse (a:tempWord),editDis)                                 -- adding a new tupel to the result list,
                          : (calcSuggestions ts tempWord word thresh acc lastrow)          -- searching through the remaining nodes on the same level,
{-                | (bestEdit < thresh) =
                            (calcSuggestions ts tempWord word thresh acc lastrow)          -- searching through the remaining nodes on the same level,
                          ++ (calcSuggestions c (a:tempWord) word thresh (acc+1) currow)   -- searching through the child nodes.
               
-}

               | otherwise = (calcSuggestions ts tempWord word thresh acc lastrow)        -- searching through the remaining nodes on the same level.
                          ++ (calcSuggestions c (a:tempWord) word thresh (acc+1) currow)   -- searching through the child nodes.
                            where
                              row     = if (isSwap (a:tempWord) word) 
                              	        then (calcRow a word [acc] lastrow)  --black magic
                              	        else (calcRow a word [acc] lastrow)                --ordinary magic 

                              currow  = reverse row
                              editDis = head row                                       -- the minimmal edit distance at the moment
                              bestEdit = editDis - ((length tempWord) - (length word)) -- the best minimal edit distance I still can achieve

                              -- the first is my tempWord(its in reverse) the second is my word that needs correction, this function is "special"
                              isSwap :: String -> String -> Bool
                              isSwap [x]        _  = False
                              isSwap (x1:x2:xs) ws = (x1 == y1) && (x2==y2)
                                  where
                       	            (y1,y2) = lastTwo ws
                                    lastTwo :: String -> (Char,Char)
                                    lastTwo []      = (' ',' ')  --there are no ' ' in seperated words so this evaluates to False
                                    lastTwo [x]     = (' ',' ')
                                    lastTwo [x1,x2] = (x1,x2)
                                    lastTwo (x:xs)  = lastTwo xs



                       	














{-calculates one row, takes the char of the node, the word we a correcting,
the current row we are calculating and the previous row-}
calcRow ::  Char -> [Char] -> [Int] -> [Int] -> [Int]
calcRow _ []     cr     lastrow      = cr
calcRow x (y:ys) (c:cr) (fir:sec:lr)
                  | (x==y)    = let ele = min fir (min (sec+1) (c+1))     --minimal element for the case sub=0,del=1,ins=1
                                in calcRow x ys (ele:c:cr) (sec:lr)
                  | otherwise = let ele = min (fir+1) (min (sec+1) (c+1)) -- minimal element for the case sub=1,del=1,ins=1
                                in calcRow x ys (ele:c:cr) (sec:lr)  

--seperates a text into its sentences 
sentences :: String -> [String]
sentences [] = []
sentences cs = go cs []
        where
            isEnd c = (c=='.') || (c=='!') || (c=='?')     -- detects and end letter for sentence 
            go :: String -> String -> [String]
            go []         ws = []
            go [c]        ws = [reverse (c:ws)]
            go (c1:c2:cs) ws
               | (isEnd c1) && (c2==' ') = (reverse (c1:ws)) : (go cs []) --end letter plus a following space is a sentence end
               | otherwise               = go (c2:cs) (c1:ws) 

--generates the trie with a wordlist
generateTree :: [String] -> [Tree Char]
generateTree ws = foldl' (insertInTree) [] ws

-- takes a word and inserts it into a given trie 
insertInTree :: [Tree Char] -> String -> [Tree Char] 
insertInTree []                [x]     = [(Node x True [])]                     --creates a new end node for the last letter
insertInTree []                (x:xs)  = [(Node x False (insertInTree [] xs))]  --creates a new child tree for the new letter
insertInTree ((Node a b c):ts) [x]     = if x == a                              --case: the last letter of the word
                                        then (Node a True c):ts                 --returns a end node for the letter and the remaining nodes on the level
                                        else (Node a b c):(insertInTree ts [x]) --keeps searching for the right node
insertInTree ((Node a b c):ts) (x:xs)  = if x == a                                 --case: a letter somewhere in the word
                                        then (Node a b (insertInTree c xs)):ts     --found the correct node, going to the child nodes  
                                        else (Node a b c):(insertInTree ts (x:xs)) --searches on the same level

-- checks if a word is in a given trie 
contains :: [Tree Char] -> String -> Bool   
contains _  [] = True
contains [] _  = False
contains ((Node a b c):ts) [x]    = if a == x             -- case: the last letter of the word
                                    then b                -- returns the isEnd value of the last node we are looking at
                                    else contains ts [x]  -- looks for other nodes on the same level that match our letter
contains ((Node a b c):ts) (x:xs) = if a == x               -- case: a letter somewhere in the word
                                    then contains c  xs     -- searches in the new child trees
                                    else contains ts (x:xs) -- searches on the same level


analyseText :: [Tree Char] -> String -> IO () -- TODO: I need an interface!!!!!
analyseText ts x = if (contains ts x) 
	               then return ()
	               else print $ "Suggestions for " ++ x ++ ": " ++ (foldl (\ string (word,minEdit) -> string ++ word ++ " " ++ (show minEdit) ++ ", ") "" (take 10 (findSuggestions ts x)))