module Main where

import Data.List (foldl')
import Text.Read (readMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Data.Char

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
    text <- mapM (wrapSentence trie) sentences1
    writeFile "corrected.txt" (unwords text)
    print "You can find your corrected text in corrected.txt"


   
-- a wraper for the analyseSentence function
wrapSentence :: [Tree Char] -> String -> IO String
wrapSentence trie sentence = analyseSentence trie [] [] sentence

{-This functions walks over a sentence and seperates words that can be send to correction,
it keeps the entire sentence split up in its parts: front + tempWord + [b] + back, so it can be 
put together when the analyseWord function needs the full sentence as context for the user-}
analyseSentence :: [Tree Char] -> String -> String -> String -> IO String
analyseSentence trie tempWord front [] = if null tempWord --This saves the last word of the last sentence from oblivion
	                                           then return front
	                                           else do
	                                           	correction <- (analyseWord trie (front ++ tempWord) tempWord) 
	                                           	return (front ++ correction)

analyseSentence trie tempWord front (b:back)
                    | isLetter b = analyseSentence trie (tempWord ++ [b]) front back  -- in the middle of a word
                   
                    | not (null tempWord) = do                                            --we found a word and send it to correction    
                    	correction <- (analyseWord trie (front ++ tempWord ++ [b] ++ back) tempWord)   
                        analyseSentence trie [] (front ++ correction ++ [b]) back
                    
                    | otherwise = analyseSentence trie [] (front ++ [b]) back   --a char that is not a letter is added to front

{-Takes a trie, a sentence and a word. First the function checks if a correction of the word is needed,
by checking if the trie contains the word, if not suggestions are calculated and the controlMenu is called. -}
analyseWord :: [Tree Char] -> String -> String -> IO String
analyseWord trie cs word
           | (contains trie word) = return word
           | otherwise            = do
                print $ "Suggestions for ->" ++ word ++ "<- in the sentence:" 
                print $ cs 

                let resultList = (findSuggestions trie word)
                correction <- (controlMenu resultList 10) 

                if correction == "" 
                then return word
                else return correction

{---Prints the control menu + suggestions and parses the user input, 
gives back "" if the suggestions where ignored, or the suggestion that the user picked.
If more is picked the controlMenu is called with (x+5) -}
controlMenu :: [(String,Int)] -> Int -> IO String 
controlMenu resultList x = do

    let modList = (modResultList x resultList)

    print $ foldl (\ string (word,index) -> string ++ word ++ " " ++ (show index) ++ ", ") "" modList
    print "Options: (i)gnore,(m)ore suggestions,pick a number"

    input <- getLine
    case input of
        "i" -> return ""
        "m" -> controlMenu resultList (x+5)
        key -> case readMaybe key :: Maybe Int of 
            Just a -> if (a<0) || (a>=length modList) 
                      then print "Not a valid input" >> controlMenu resultList x
                      else let (result,_) = (modList !! a ) in return result
            Nothing -> print "Not a valid input" >> controlMenu resultList x


{- This function removes the minimum edit distance and replaces it with an index,
returns x elements of the list, or the entire list-}
modResultList ::  Int -> [(String,Int)] -> [(String,Int)] 
modResultList x resultList =  if (x>len) 
                              then makeIndex [0..len] (take len resultList) 
                              else makeIndex [0..x] (take x resultList)
	                                  where
	                                 	len = length resultList
	                                 	makeIndex _     []          = []
	                                 	makeIndex (y:ys) ((a,b):xs) = (a,y) : makeIndex ys xs

-- a wrapper for the calcSuggestions function
findSuggestions :: [Tree Char] -> String -> [(String,Int)]
findSuggestions ts word = sortBy (comparing snd) (calcSuggestions ts [] word 8 1 [] [0..(length word)])

{- calcSuggestions trie [] "Halo" 8 1 [0..(length "Halo")]
Parameters: Trie , the tempWord we build up while traversing the trie, the word we want suggestions for, 
the treshold for the minimum Editdistance, a accu, previous row, last row 

This is the main function that calculates the suggestions for a word, it works it way
through the trie and adds all suggestions to the result list, that have a minimum edit 
distance that is lesser than the treshhold.-}
calcSuggestions :: [Tree Char] -> String -> String -> Int -> Int -> [Int] -> [Int] -> [(String,Int)] 
calcSuggestions []                _        _    _      _   _       _ = []
calcSuggestions ((Node a b c):ts) tempWord word thresh acc prevrow lastrow
               | b  && (editDis < thresh)=                                                 -- word end and minimum Editdistance smaller than the threshold,
                             (reverse (a:tempWord),editDis)                                -- adding a new tupel to the result list,
                          :  (calcSuggestions ts tempWord word thresh acc prevrow lastrow)         -- searching through the remaining nodes on the same level,
                          ++ (calcSuggestions c (a:tempWord) word thresh (acc+1) lastrow currow)   -- searching through the child nodes.
            
               | otherwise = (calcSuggestions ts tempWord word thresh acc prevrow lastrow)        -- searching through the remaining nodes on the same level.
                          ++ (calcSuggestions c (a:tempWord) word thresh (acc+1) lastrow currow)   -- searching through the child nodes.
                            where
                              row     =  
                                         if (isSwap tempWord word)
                                         then reverse (replaceElement revPosition revEle  (reverse (calcRow a word [acc] lastrow)))
                                         else (calcRow a word [acc] lastrow)                
                              currow  = reverse row
                              editDis = head row   -- the minimmal edit distance at the moment
                              
                              revPosition = length tempWord
                              revEle = (prevrow !! (revPosition - 2)) + 1

replaceElement :: Int -> Int -> [Int] -> [Int]
replaceElement 0 x (y:ys) = if x < y then recalc (x : ys) else y : ys  
replaceElement n x (y:ys) = y : (replaceElement (n-1) x ys)
                                                   
recalc :: [Int] -> [Int]
recalc [x]    = [x]
recalc (x1:x2:xs) = if ((x1+1)<x2) then x1 : (recalc ((x1+1):xs)) else x1:x2:xs 


isSwap :: String -> String -> Bool
isSwap _       []         = False
isSwap []      _          = False 
isSwap _       [y]        = False
isSwap [x]     _          = False 
isSwap [x1,x2] (y1:y2:ys) = (x1==y2) && (x2==y1)
isSwap (x:xs)  (y:ys)     = isSwap xs ys 



                             
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