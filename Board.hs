module Board where

mkBoard m n 
  | m > 0 = take n (repeat 0) : mkBoard (m-1) n
  | m == 0 = []
  | otherwise = []

mkPlayer = 1
mkOpponent = 2

iterBoard :: [[Integer]] -> Integer -> Integer -> [[Integer]]
iterBoard bd p i
  | length bd == 0 = []
  | i == 7 = (dropInSlot (head bd) p) : iterBoard (tail bd) p (i+1)
  | otherwise = (head bd) : iterBoard (tail bd) p (i+1)

dropInSlot [0] x = [x]
dropInSlot l n 
  | (head l) /= 0 = l
  | (head (tail l)) /= 0 && (head l == 0) = n : tail l
  | otherwise = (head l) : dropInSlot (tail l) n

isSlotOpen :: [[Integer]] -> Integer -> Bool
isSlotOpen bd i
  | i >= toInteger (numSlot bd) = head (head bd) == 0
  | i /= toInteger (numSlot bd) = isSlotOpen (tail bd) (i + 1)
  | otherwise = False

numSlot :: [[Integer]] -> Int
numSlot bd = length bd

isFull bd = length (filter (/=0) (map head bd)) == numSlot bd

consecutive lst p
  | length lst <= 3 = False
  | head lst == p && head (tail lst) == p && head (tail (tail lst)) == p && head (tail (tail (tail lst))) == p = True
  | otherwise = consecutive (tail lst) p

rowWon bd p
  | head bd == [] = False
  | consecutive (map head bd) p = True
  | otherwise = rowWon (map tail bd) p

colWon bd p
  | bd == [] = False
  | consecutive (head bd) p = True
  | otherwise = colWon (tail bd) p

isWonBy bd p = colWon bd p || rowWon bd p

playerToChar p
  | p == mkPlayer = " O "
  | p == mkOpponent = " X "
  | otherwise = " . "

boardToStr bd
  | head bd == [] = ""
  | otherwise = getRow (map head bd) ++ boardToStr (map tail bd)

getRow row = (concat (map (playerToChar) row)) ++ "\n"
