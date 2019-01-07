inc:: Int -> Int
inc x = x + 1

average :: Float -> Float -> Float
average a b = (a+b) / 2

showResult:: Int -> String
showResult a = "The result is " ++ show a

addMul :: Num a => a -> a -> (a, a)
addMul x y = (x + y, x * y)



type Point = (Int, Int)

moveHori :: Point -> Int -> Point
moveHori (x,y) distance = (x+distance, y)
moveVert :: Point -> Int -> Point
moveVert (x,y) distance = (x, y+distance)


type Colour = String
type ColourPoint = (Int, Int, Colour)
origin :: Colour -> ColourPoint
origin colour  = (0, 0, colour)

move :: Point -> Int -> Int -> Point
move (x, y) xdist ydist = (x+xdist, y+ydist)

distance :: ColourPoint -> ColourPoint -> Float
distance (x1, y1, colour1) (x2, y2, colour2) 
  = sqrt (fromIntegral (dx * dx + dy * dy))
  where
    dx = x2 - x1
    dy = y2 - y1

colourOfPoint:: ColourPoint-> Colour
colourOfPoint (x, y, colour) = colour


firstTenPrimes :: [Int]
firstTenPrimes  = [2, 3, 5, 7, 11, 13, 17, 19, 23, 27]

oneToTwenty :: [Int]
oneToTwenty = [1..20]

sort2 :: Int -> Int -> (Int, Int)
sort2 a b
    | a > b = (a, b)
    | otherwise = (b, a)
    
isLower :: Char -> Bool
isLower x = elem x ['a', 'z']

mangle :: String -> String
mangle (x:xs) = xs ++ [x]
mangle [] = []

divide :: Int -> Int -> Int
divide x y = length [x, x+x.. y]

natSum :: (Num a, Ord a) => a -> a
natSum 0              = 0
natSum n  | n > 0     = n + natSum (n - 1) 
          | otherwise = error "natSum: Input value too small!"

fact :: Int -> Int
fact n
    | n==0 = 1
    | n>0 = n * fact (n-1)
    | otherwise = error "natSum: Input value too small!"
    
countOdds :: [Int] -> Int
countOdds [] = 0
countOdds (x:xs) 
    | rem x 2 ==1 = 1 + countOdds xs
    | otherwise = countOdds xs