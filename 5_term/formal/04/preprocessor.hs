import qualified Data.Map as Map

options :: Map.Map Char Char -> String -> [String]
options maps (x:y:xs)
    | x == '#' =
        if Map.member y maps
            then map ((maps Map.! y):) $ options maps xs
            else (map ('0':) $ options (Map.insert y '0' maps) xs) ++
                    (map ('1':) $ options (Map.insert y '1' maps) xs)
options maps (x:xs) = map (x:) $ options maps xs
options maps [] = [[]]

main = do
    input <- getContents
    putStr $ unlines $ concatMap (options Map.empty) $ lines input
