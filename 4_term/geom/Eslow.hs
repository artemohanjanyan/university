import Text.Printf
import Data.List as List
import Control.Monad

solve :: [(Int, Int)] -> [(Int, Int)]
solve points =
    let
        comparePoints (x1, y1) (x2, y2)
            | x1 < x2    = LT
            | x1 > x2    = GT
            | y1 < y2    = LT
            | otherwise  = GT
            
        sorted = List.sortBy comparePoints points
        reversed = reverse sorted
        
        area (x1, y1) (x2, y2) (x3, y3) =
            (x2 - x1) * (y2 + y1) + (x3 - x2) * (y3 + y2) + (x1 - x3) * (y1 + y3)

        pushPoint []                          point = [point]
        pushPoint stack @ (first:[])          point = point : stack
        pushPoint stack @ (first:second:rest) point
            | area second first point > 0           = point : stack
            | otherwise                             = pushPoint (second : rest) point

        top = foldl' pushPoint [] sorted
        bottom = foldl' pushPoint [] reversed
    in 
        tail top ++ tail bottom

main :: IO ()
main = do
  n <- readLn :: IO Int
  points <- replicateM n $ do
    nums <- getLine
    let [a, b] = map (read :: String -> Int) $ words nums
    return (a, b)
  let ans = solve points
  putStrLn $ show $ length ans
  let printPoint (x, y) = putStrLn $ show x ++ " " ++ show y
  mapM_ printPoint ans
