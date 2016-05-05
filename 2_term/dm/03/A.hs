import qualified System.Random as R
import qualified Data.Maybe as M

data Treap =
    Node (Int, Int) Treap Treap |
    Null

instance Show Treap where
    show (Node (x, _) l r) = show l ++ " " ++ show x ++ " " ++ show r
    show Null = ""

merge :: Treap -> Treap -> Treap
merge Null b = b
merge b Null = b
merge left@(Node (x1, y1) l1 r1) right@(Node (x2, y2) l2 r2)
    | y1 < y2 = Node (x1, y1) l1 $ merge r1 right
    | otherwise = Node (x2, y2) (merge left l2) r2

split :: Int -> Treap -> (Treap, Treap)
split _ Null = (Null, Null)
split key (Node (x, y) l r)
    | x < key =
        let (l1, r1) = split key r
        in  (Node (x, y) l l1, r1)
    | otherwise =
        let (l1, r1) = split key l
        in  (l1, Node (x, y) r1 r)

insert :: (Int, Int) -> Treap -> Treap
insert p Null = Node p Null Null
insert (x, y) t = 
    let (t1, t2) = split x t
    in  merge t1 $ merge (singletone x y) t2

delete :: Int -> Treap -> Treap
delete x t =
    let (ll, r1) = split x t
        (_, rr) = split (x + 1) r1
    in  merge ll rr

next :: (Int -> Bool) -> Treap -> Maybe Int
next _ Null = Nothing
next p v@(Node (x, y) l r)
    | p x = pick x nextL
    | otherwise = next p r
    where
        nextL = next p l
        pick x Nothing = Just x
        pick _ j = j

prev :: (Int -> Bool) -> Treap -> Maybe Int
prev _ Null = Nothing
prev p v@(Node (x, y) l r)
    | p x = prev p l
    | otherwise = pick x prevR
    where
        prevR = prev p r
        pick x Nothing = Just x
        pick _ j = j

exists :: Int -> Treap -> Bool
exists key t = 
    let test (Just x) = key == x
        test Nothing = False
    in  test $ next (>= key) t

singletone x y = Node (x, y) Null Null

main = do
    gen <- R.newStdGen
    content <- getContents
    --content <- readFile "bst.in"
    let
        input = map (\[a, b] -> (a, read b :: Int)) $ map words $ lines content
        iter (t, gen, ans) ("insert", x) = 
            let (y, newGen) = R.next gen
            in  (insert (x, y) t, newGen, ans)
        iter (t, gen, ans) ("delete", x) = (delete x t, gen, ans)
        iter (t, gen, ans) ("exists", x) =
            let ifExists = exists x t
                ansWord True = "true"
                ansWord False = "false"
            in  (t, gen, ans ++ ansWord ifExists ++ "\n")
        iter (t, gen, ans) ("next", x) = 
            let nextV = next (> x) t
                ansWord (Just x) = show x
                ansWord Nothing = "none"
            in  (t, gen, ans ++ ansWord nextV ++ "\n")
        iter (t, gen, ans) ("prev", x) = 
            let prevV = prev (>= x) t
                ansWord (Just x) = show x
                ansWord Nothing = "none"
            in  (t, gen, ans ++ ansWord prevV ++ "\n")
        (_, _, output) = foldl iter (Null, gen, "") input
    putStr output
    --writeFile "bst.out" output
