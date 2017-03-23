import Text.Printf
import Data.List (inits)
import qualified Data.Set as Set
import qualified Data.Map as Map

maxN = 30 :: Int
maxLength = (round $ log (fromIntegral $ product [1..(toInteger maxN)]) / log 2 :: Int) + 1

numbers = [1..maxN]
facts = map (\x -> product [1..(toInteger x)]) numbers

main = do
    putStrLn "start: init"
    putStrLn "accept: accept"
    putStrLn "reject: reject"
    putStrLn "blank: _"

    putStrLn "init 0 -> init 0 >"
    putStrLn "init 1 -> init 1 >"
    putStrLn "init _ -> fill0 | >"

    mapM_ putStrLn $ map
        (\l -> printf "fill%d _ -> fill%d c%d >" (l - 1) l (l - 1))
        [1..(maxLength - 1)]
    putStrLn $ printf "fill%d _ -> comeBack c%d <" (maxLength - 1) (maxLength - 1)

    mapM_ putStrLn $ map
        (\l -> printf "comeBack c%d -> comeBack c%d <" l l)
        [0..(maxLength - 1)]
    putStrLn "comeBack | -> comeBack | <"
    putStrLn "comeBack 0 -> comeBack 0 <"
    putStrLn "comeBack 1 -> comeBack 1 <"
    putStrLn "comeBack _ -> readNum  _ >"

    let factStrings :: [(Int, String)]
        factStrings = zip [1..] (map (\f -> printf "%b" f) facts :: [String])
    let numStrings = "" : map (\f -> printf "%b" f) numbers :: [String]
    let numStringSet = Set.fromList numStrings

    let printReadNum :: (Integer, String) -> IO ()
        printReadNum (num, str) = do
            if Set.member (str ++ "0") numStringSet then
                putStrLn $ printf "readNum%s 0 -> readNum%s0 _ >" str str
            else
                putStr ""
            if Set.member (str ++ "1") numStringSet then
                putStrLn $ printf "readNum%s 1 -> readNum%s1 _ >" str str
            else
                putStr ""
            if str /= "" then
                putStrLn $ printf "readNum%s | -> writeNum%d _ >" str num
            else
                putStr ""
    mapM_ printReadNum $ zip [0..] numStrings

    let printWrites :: Int -> IO ()
        printWrites n = do
            let printWrite (num, str) = do
                if n < length str then
                    putStrLn $ printf "writeNum%d c%d -> writeNum%d %c >" num n num (str !! n)
                else
                    putStrLn $ printf "writeNum%d c%d -> writeNum%d _ >" num n num
            mapM_ printWrite factStrings
    mapM_ printWrites [0..(maxLength - 1)]

    let printFinish :: Int -> IO ()
        printFinish n = do
            putStrLn $ printf "writeNum%d _ -> finish1 _ ^" n
    mapM_ printFinish numbers
    putStrLn "finish1 _ -> finish1 _ <"
    putStrLn "finish1 0 -> finish2 0 <"
    putStrLn "finish1 1 -> finish2 1 <"
    putStrLn "finish2 0 -> finish2 0 <"
    putStrLn "finish2 1 -> finish2 1 <"
    putStrLn "finish2 _ -> accept _ >"
