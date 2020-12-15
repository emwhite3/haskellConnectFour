module Main where
import Board

readSlot bd p = do
  putStrLn "Enter a slot between 1 and 7: "
  line <- getLine
  let parsed = reads line :: [(Integer, String)] in
    if length parsed == 0
    then invalid bd p
    else let (x, _) = head parsed in
      if x > 0 && x < 8 && isSlotOpen bd x
      then return x
      else invalid bd p
  where
    invalid bd p = do
      putStrLn "Invalid input!"
      readSlot bd p

play :: [[Integer]] -> IO()
play bd 
  | isWonBy bd mkPlayer = putStrLn "Player 1 Won"
  | isWonBy bd mkOpponent = putStrLn "Player 2 Won"
  | isFull bd = putStrLn "Draw"
  | otherwise = do
    putStrLn "Player"
    x <- readSlot bd mkPlayer
    let newOne = iterBoard bd 1 x
    putStrLn "Opponent"
    y <- readSlot newOne mkOpponent
    let new = iterBoard newOne 2 y
    putStrLn (boardToStr new)
    play new

main :: IO ()
main = do
  play (mkBoard 7 6)
