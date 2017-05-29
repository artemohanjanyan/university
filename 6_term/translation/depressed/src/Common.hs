module Common where

import Grammar

import Control.Monad.State.Strict
import Data.Either (isRight)
import Data.List (tails)
import qualified Data.Set        as Set
import qualified Data.Map.Strict as Map

data Epsilon    = Epsilon    deriving (Show, Eq, Ord)
type FirstValue = Either Epsilon Terminal
type First      = Map.Map NonTerminal (Set.Set FirstValue)

data EndOfInput  = EndOfInput deriving (Show, Eq, Ord)
type FollowValue = Either EndOfInput Terminal
type Follow      = Map.Map NonTerminal (Set.Set FollowValue)

runFirst :: [Symbol] -> First -> Set.Set FirstValue
runFirst []                           _     = Set.singleton $ Left Epsilon
runFirst (Symbol (Right term) : _)    _     = Set.singleton $ Right term
runFirst (Symbol (Left nonTerm) : ss) first =
    let sFirst = Map.findWithDefault Set.empty nonTerm first
    in if Set.member (Left Epsilon) sFirst
        then Set.union sFirst (runFirst ss first)
        else sFirst

makeFirst :: Grammar -> First
makeFirst (Grammar grammar) = snd $ runState run Map.empty
  where
    run :: State First ()
    run = do
        changed <- mapM runRule grammar
        if or changed
            then run
            else pure ()

    runRule :: Rule -> State First Bool
    runRule (nonTerm :-> (symbols, _)) = do
        first <- get
        let nonTermFirst = Map.findWithDefault Set.empty nonTerm first
        let newNonTermFirst = Set.union nonTermFirst (runFirst symbols first)
        if nonTermFirst == newNonTermFirst
            then pure False
            else do
                modify (Map.insert nonTerm newNonTermFirst)
                pure True

makeFollow :: Grammar -> First -> Follow
makeFollow (Grammar grammar) first = snd $ runState run initState
  where
    initState :: Follow
    initState =
        let (start :-> _) = head grammar
        in Map.singleton start $ Set.singleton $ Left EndOfInput

    cases :: [(NonTerminal, NonTerminal, [Symbol])]
    cases = do
        (a :-> (symbols, _)) <- grammar
        (Symbol b' : gamma) <- init $ tails symbols
        case b' of
            Left b -> pure (a, b, gamma)
            _      -> []

    run :: State Follow ()
    run = do
        changed <- mapM runCase cases
        if or changed
            then run
            else pure ()

    runCase :: (NonTerminal, NonTerminal, [Symbol]) -> State Follow Bool
    runCase (a, b, gamma) = do
        let gammaFirst = runFirst gamma first
        follow <- get
        let bFollow = Map.findWithDefault Set.empty b follow
        let gammaFirstAsFollow = Set.map firstToFollow $ Set.filter isRight gammaFirst
        let bFollow1 = Set.union bFollow gammaFirstAsFollow
        let bFollow2 = if Set.member (Left Epsilon) gammaFirst
            then Set.union bFollow1 $ Map.findWithDefault Set.empty a follow
            else bFollow1
        if bFollow == bFollow2
            then pure False
            else do
                modify (Map.insert b bFollow2)
                pure True

firstToFollow :: FirstValue -> FollowValue
firstToFollow (Left Epsilon) = Left EndOfInput
firstToFollow (Right x)      = Right x
