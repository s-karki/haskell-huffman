--Build the inverted index given an input list of lists 

inverted_index :: [[String]] -> [(String, [Int])] 
inverted_index book = map (makeNum book) (construct_pairs (unique_words book))


--Make a list of numbers that tells us what pages a word in an ordered pair appear in 
--from a list of booleans [["f"] ["c"] ["f"]] -> ("f", []) -> [0, 2]
makeNum :: [[String]] -> (String, [Int]) -> (String, [Int])
makeNum book pair = (fst(pair), makeNumFromBool pair (makeBool book pair))

--Make a list of booleans that tell us if a word in an ordered pair is in one of the 
--list elements e.g. [["f"] ["c"] ["a"]] -> ("f", []) -> [T, F, F]

makeBool :: [[String]] -> (String, [Int]) -> [Bool]
makeBool book pair = map (boolf pair) book

boolf :: (String, [Int]) -> [String] -> Bool
boolf pair [] = False
boolf pair (x : xs)
  | fst(pair) == x = True 
  | otherwise = boolf pair xs

--Make a list of numbers that tells us what pages a word in an ordered pair appear in 
--from a list of booleans 
--e.g. ("f", []) -> [T, F, T] -> [0, 2]
makeNumFromBool :: (String, [Int]) -> [Bool] -> [Int]
makeNumFromBool pair bools = filter (/= -1) 
                            (zipWith (zipfunc) bools (take (length bools) (zoomUp 0)))

zipfunc x y
 | x == True = y
 | otherwise = -1
 
zoomUp x = x : zoomUp(x + 1)
 
--TODO zoomUp, filter ( /= -1 ) list 

{- construct a list of ordered pairs, where the first element in each pair is a word that
occurs atleast once in the input list. The second element will be the list of pages the word
appears on. Input is the list of unique words in the input list return value of 
unique_words -}
construct_pairs :: (Eq a, Ord a) => [a] => [(a, [b])]
construct_pairs list = zip list (replicate (length list) [])
  

{- return a list of words that occur once or more from a list of lists. We do this by 
folding the list into a single list, sorting the list of all elements, 
and removing duplicates
-}
unique_words :: (Eq a, Ord a) => [[a]] -> [a]
unique_words list = removeDupsHelp (sort (foldr (++) [] list))

removeDupsHelp :: (Eq a) => [a] -> [a]
removeDupsHelp [] = []
removeDupsHelp [s] = [s]
removeDupsHelp (x : y : ys)
  | x == y = removeDupsHelp(y : ys)
  | otherwise = x : removeDupsHelp(y : ys)
  

  

sort :: Ord a =>  [a] -> [a]
sort [] = []
sort a = foldl sort_func [] a 

sort_func :: Ord a => [a]-> a -> [a]
sort_func [] a = [a]
sort_func b a = foldr order_func [a] b

order_func :: Ord a => a-> [a] -> [a]
order_func x (y:ys)
   | x>y = y:x:ys
   |otherwise = x:y:ys
   
   
   
   
   
{-   
removeDups :: (Ord a) => [a] -> [a]
removeDups list = removeDupsHelp (sort list)



--for each ordered pair, we traverse through each element in each list (page) of the book
index_func :: [[String]] -> (String, [Int]) -> (String, [Int])
index_func book pair  = update ( map ( filter (\x -> x == fst(pair)) ) book ) pair 0   


--update the pair, given an lists of lists, where the size of each list tells us the number of times
--the pair word occurs in that list
update :: [[String]] -> (String, [Int]) -> Int -> (String, [Int])
update [] pair n = pair
update (x : xs) pair n
    | length(x) > 0 = ( fst(pair), (snd(pair) ++ [n] ++ snd(update xs pair (n + 1))  ))
    | otherwise = ( fst(pair, snd(pair) ++ snd(update xs pair (n + 1))))
--}