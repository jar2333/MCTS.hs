-- stack --resolver lts-19.23 ghci

module MCTS
    ( 
        Player(..),
        opposing,

        GameState,
        next,
        eval,
        pick,
        sim,
        
        mcts, 
        drawGameTree,

        testMCTS,
        testStep
    ) where


{-# LANGUAGE InstanceSigs #-}
import Control.Monad.State
import Data.Tree
import Data.List(sortBy, maximumBy)
import Data.Function
import Control.Parallel.Strategies

data Player = One | Two | Tie deriving (Eq, Show)

opposing :: Player -> Player
opposing One = Two
opposing Two = One
opposing Tie = Tie

class GameState g where  
    next :: g -> [g]    -- gets gamestates available from given
    eval :: g -> Maybe(Player) -- determines the winner of the given gamestate
    pick :: g -> [g] -> g    -- picks a gamestate from a list of available ones given parent gamestate

    sim :: g -> Player  -- gets a winner of a game state (default is a simulation)
    sim g = case next g of
                [] -> case eval g of 
                         Just(result) -> result
                         Nothing      -> Tie --if nothing new is possible, and no clear outcome, it's a Tie

                gs -> let picked = pick g gs 
                      in case eval picked of 
                         Just(result) -> result
                         Nothing      -> sim picked 

-- Number of iterations, number of rollouts, initial player, starting state
mcts :: GameState g => Int -> Int -> Player -> g -> g
mcts iter rollout first s = gameState choice
    where choice = getGameData $ maximumBy (compare `on` getScore . getGameData) ch
          Node _ ch = applyNtimes iter (step rollout) $ root first s


-----------------------------------------
-- MCTS Data Structures 
-----------------------------------------

-- Holds info relevant to MCTS in each node of the tree
data GameData g = GameData {wins :: Int, total :: Int, player :: Player, gameState :: g}

showGameData :: Show g => GameData g -> String
showGameData (GameData w t p g) = show w ++ ", " ++ show t ++ ", " ++ show p ++ ", " ++ show g

getScore :: GameData g -> Double
getScore GameData{wins=w, total=t} = (fromIntegral w) / (fromIntegral t) :: Double

type GameResult = Player

-- Takes a game result specifying the win amount and player who won to update game data
updateWins :: GameResult -> GameData g -> GameData g
updateWins pl (GameData w t p g)  
    | p == pl   = GameData (w+1) (t+1) p g
    | otherwise = GameData w (t+1) p g

-- The MCTS game tree which is incrementally created
type GameTree g = Tree (GameData g)

drawGameTree :: Show g => Tree (GameData g) -> String
drawGameTree = drawTree . fmap showGameData

getGameData :: GameTree g -> GameData g
getGameData = rootLabel

root :: Player -> g -> GameTree g
root p g = Node (GameData 0 0 p g) []

-----------------------------------------
-- MCTS Algorithm 
-----------------------------------------

-- Number of rollouts, and a game tree
step :: GameState g => Int -> GameTree g -> GameTree g
step r t = evalState (walk t) ([], r)

ucb :: GameData g -> GameData g -> Double
ucb GameData{total=p_total} GameData{wins=c_wins, total=c_total} = (w / n) + c * sqrt (log np / n)
    where n  = fromIntegral c_total --infinity when 0: desired behavior!
          np = fromIntegral p_total
          w  = fromIntegral c_wins
          c  = sqrt (2.0 :: Double)

-- Returns a list of child GameData from given GameData
possibleMoves :: GameState g => GameData g -> State ([GameResult], Int) [GameData g]
possibleMoves GameData{gameState=g, player=p} = do
    (_, rollout) <- get

    let states  = next g 

    let results = map sim (take rollout states) `using` parList rseq --parallelism

    let (simulated, other) = splitAt rollout [GameData 0 0 (opposing p) gs | gs <- states ]
    let updated = zipWith updateWins results simulated

    put (results, rollout)

    return $ updated ++ other
          

-- Returns a list of children tree nodes created from given node's game state
expand :: GameState g => GameTree g -> State ([GameResult], Int) [GameTree g]
expand (Node d ch) = do
    if total d == 0 --if never visited
        then return [] --we create no children (part of MCTS)
        else do 
            moves <- possibleMoves d
            let newChildren = [ Node cd [] | cd <- moves]
            return $ ch++newChildren    

-- Takes a tree, traverses to a leaf using UCB, then expands it
-- Use the state monad to propagate upwards the list of winning player in simulated/evaluated nodes as a state
walk :: GameState g => GameTree g -> State ([GameResult], Int) (GameTree g)

-- Leaf node
walk n@(Node d []) = do 
    newChildren <- expand n 
    case newChildren of
        [] -> do -- If no tree children created, simulate game from leaf and update it with results
            let result = sim $ gameState d 
            (_, rollout) <- get
            put ([result], rollout)
            let updatedData = updateWins result d
            return $ Node updatedData []
        ch -> do -- If tree children created, some games were simulated. Update current node with sim results.
            (results, _) <- get
            let updatedData = foldr updateWins d results
            return $ Node updatedData ch

-- Branch node
walk (Node d ch) = do
    updatedChild    <- walk selected
    (results, _)    <- get
    let children    = updatedChild:rest
    let updatedData = foldr updateWins d results
    return $ Node updatedData children
    where 
          selected:rest = sortBy compareUCB ch
          compareUCB = compare `on` ((*(-1)) . ucb d . getGameData) --max instead of min hence *-1

----------------------------
-- TESTING
----------------------------

testStep :: (Show g, GameState g) => Int -> Int -> Player -> g -> IO ()
testStep n r p g = (putStrLn . drawGameTree) (applyNtimes n (step r) $ root p g)

testMCTS :: (Show g, GameState g) => Int -> Int -> Player -> g -> IO ()
testMCTS n r p g = print $ mcts n r p g


----------------------------
-- Helpers
----------------------------

-- From https://gist.github.com/thekarel/9964975
applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)
