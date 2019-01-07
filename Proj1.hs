{-
Cao Peng
787664
Declarative Programming
COMP30020/COMP90048
-}

module Proj1 (initialGuess, nextGuess, GameState) where
import Data.List
import Data.Char

type GameState = [[String]]
pitches = ["A1", "B1", "C1", "D1", "E1", "F1", "G1",
           "A2", "B2", "C2", "D2", "E2", "F2", "G2",
           "A3", "B3", "C3", "D3", "E3", "F3", "G3"]

initialGuess :: ([String],GameState)
initialGuess = (useDeleteMid(nub(sortTriples (triples pitches))))

nextGuess :: ([String],GameState)->(Int,Int,Int)->([String], GameState)
nextGuess ([],[]) (_,_,_) = ([],[])
nextGuess (prevGuess,[]) (_,_,_) = ([],[])
nextGuess ([],gameState) (_,_,_) = (gameState!!0, gameState)
nextGuess (prevGuess, gameState) (cp, cn, co) = useDeleteMid(analyse gameState prevGuess (cp, cn, co))

-- combination of the pitches into triples
triples :: [String] -> [[String]]
triples xs = filter ((3==).length.nub) $ mapM (const pitches) [1..3]

-- sort the triples so that duplicates can be removed with nub
sortTriples :: [[String]] -> [[String]]
sortTriples [] = []
sortTriples (x:xs) = sort x : sortTriples xs

-- index the 0 for note, index 1 for octive
noteOrOctiveList :: [String] -> Int -> [Char]
noteOrOctiveList [a, b, c] n = [a!!n, b!!n, c!!n]

-- the common elements in two lists
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' xs ys = xs \\ (xs \\ ys)

-- the number of commons in two cords
matches :: Eq a => [a] -> [a] -> Int
matches xs ys = length (intersect' xs ys)

-- remove all cords with less commons with prevGuess than the int specified by the 4th argument
deleteWrongPitch:: GameState -> [String] -> Int -> Int -> Int -> GameState
deleteWrongPitch [] [cp, cn, co] noteOrOct nMatch nCorrect= []
deleteWrongPitch (x:xs) [cp, cn, co] noteOrOct nMatch nCorrect
    | (matches (noteOrOctiveList x noteOrOct) (noteOrOctiveList [cp, cn, co] noteOrOct)) >= (nMatch+nCorrect) = x: deleteWrongPitch xs [cp, cn, co] noteOrOct nMatch nCorrect
    | otherwise = deleteWrongPitch xs [cp, cn, co] noteOrOct nMatch nCorrect

-- remove all cords based on the knowledge of no. of correct pitches
deleteNotContainCorrect :: GameState -> [String] -> Int -> GameState
deleteNotContainCorrect [] [p1, p2, p3] cp = []
deleteNotContainCorrect (x:xs) [p1, p2, p3] cp
    | matches x [p1, p2, p3] == cp = x: deleteNotContainCorrect xs [p1, p2, p3] cp
    | otherwise = deleteNotContainCorrect xs [p1, p2, p3] cp

-- update gameState by reducing the possible candidate cords
analyse :: GameState -> [String] -> (Int, Int, Int) -> GameState
analyse [] [] (_) = []
analyse gameState [] (_) = []
analyse [] prevGuess (_) = []
analyse gameState prevGuess (cp,cn,co) =
    deleteWrongPitch
        (deleteWrongPitch
            (deleteNotContainCorrect gameState prevGuess cp) prevGuess 0 cn cp) prevGuess 1 co cp

-- retieve the element in the mid of the array, and delete that element from the array
useDeleteMid :: Eq a => [a] -> (a, [a])
useDeleteMid array = (array!!(div (length array) 2), delete (array!!(div (length array) 2)) array)