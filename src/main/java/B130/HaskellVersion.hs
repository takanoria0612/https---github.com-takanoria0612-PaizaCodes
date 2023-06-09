import Control.Monad (replicateM)

type Board = [[Int]]
type Block = [[Int]]

main :: IO ()
main = do
    putStrLn "Starting program..."
    [n, m] <- fmap (map read . words) getLine
    board <- readBoard n
    block <- readBlock
    putStrLn $ if canInsertBlock board block then "Yes" else "No"

readBoard :: Int -> IO Board
readBoard n = fmap (map (map (\c -> if c == '#' then 1 else 0))) (replicateM n getLine)

readBlock :: IO Block
readBlock = fmap (map (map (\c -> if c == '#' then 1 else 0))) (replicateM 3 getLine)

canInsertBlock :: Board -> Block -> Bool
canInsertBlock board block = any (canPlaceBlockAt board block) positions
  where
    positions = [(x, y) | x <- [0 .. length board - 1], y <- [0 .. length (head board) - 1]]
    rotations = take 4 $ iterate rotateBlock block

    canPlaceBlockAt :: Board -> Block -> (Int, Int) -> Bool
    canPlaceBlockAt board block (x, y) = any (\b -> isValid board b (x, y)) rotations

    isValid :: Board -> Block -> (Int, Int) -> Bool
    isValid board block (x, y) = all check indices
      where
        indices = [(i, j) | i <- [0 .. 2], j <- [0 .. 2]]
        check (i, j)
          | block !! i !! j == 1 =
            x + i < length board && y + j < length (head board) && board !! (x + i) !! (y + j) == 0
          | otherwise = True

rotateBlock :: Block -> Block
rotateBlock block = [[block !! (2 - j) !! i | j <- [0 .. 2]] | i <- [0 .. 2]]
