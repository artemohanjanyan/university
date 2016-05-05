import Text.Printf
import Data.List as List

main :: IO ()
main = do
   --content <- getContents
   content <- readFile "shooter.in"
   let
      input = map words $ lines content
      [n, m, k] = map (read::String->Int) (input !! 0)
      inputProbs = map (read::String->Double) (input !! 1)

      missProbs = map (** ((fromIntegral::Int->Double) m)) $ map (1.0 -) inputProbs
      ans = (missProbs !! (k - 1)) / sum missProbs
   --putStrLn $ show $ ans
   writeFile "shooter.out" $ show ans
