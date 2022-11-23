import Control.Monad.State
import Control.Monad.Writer
import Data.Tree
import Data.List
import Data.Function


data Player = One | Two | Tie deriving (Eq)

opposite :: Player -> Player
opposite One = Two
opposite Two = One
opposite Tie = Tie

class GameState g where  
    next :: g -> [g]    -- gets gamestates available from given
    eval :: g -> Player -- determines the winner of the given gamestate
    pick :: [g] -> g    -- picks a gamestate from a list of available ones

    sim :: g -> Player  -- gets a winner of a game state (default is a simulation)
    sim g = case next g of
        [] -> eval g 
        gs -> sim $ pick gs

data GameData g = GameData {wins :: Int, total :: Int, player :: Player, gameState :: g}

type GameResult = Player

-- Takes a game result specifying the win amount and player who won to update game data
updateWins :: GameResult -> GameData g -> GameData g
updateWins pl (GameData w t p g)  
    | p == pl   = GameData (w+1) (t+1) p g
    | otherwise = GameData w (t+1) p g

type GameTree g = Tree (GameData g)

getGameData :: GameTree g -> GameData g
getGameData = rootLabel

mcts :: GameState g => GameState g => g -> g
mcts s = s

ucb :: GameState g => GameData g -> GameData g -> Double
ucb GameData{total=p_total} GameData{wins=wins, total=c_total} = (w / n) + c * sqrt (log np / n)
    where n  = fromIntegral (c_total + 1) -- to avoid division by 0?
          np = fromIntegral p_total
          w  = fromIntegral wins
          c  = sqrt (2.0 :: Double)

-- Returns a list of child GameData from given GameData
possibleMoves :: GameState g => GameData g -> State [GameResult] [GameData g]
possibleMoves d@GameData{gameState=g, player=p} = do
    put results
    return $ zipWith updateWins results moveData
    where 
          moveData = [GameData 0 0 (opposite p) gs | gs <- states ]
          results  = map sim states
          states   = take 1 $ next g -- magic number, change later

-- Returns a list of children tree nodes created from given node's game state
expand :: GameState g => GameTree g -> State [GameResult] [GameTree g]
expand (Node d ch) = do
    moves <- possibleMoves d
    let newChildren = [ Node d [] | d <- moves]
    return $ ch++newChildren    

-- Takes a tree, traverses to a leaf using UCB, then expands it
-- Use the state monad to propagate upwards the list of winning player in simulated/evaluated nodes as a state
walk :: GameState g => GameTree g -> State [GameResult] (GameTree g)
walk n@(Node d []) = do 
    newChildren <- expand n 
    case newChildren of
        [] -> do
            let result = eval $ gameState d 
            put [result]
            let updatedData = updateWins result d
            return $ Node updatedData []
        ch -> do
            results <- get
            let updatedData = foldr updateWins d results
            return $ Node updatedData ch

walk (Node d ch) = do
    updatedChild    <- walk selected
    results         <- get
    let children    = updatedChild:rest
    let updatedData = foldr updateWins d results

    return $ Node updatedData children
    where 
          selected:rest = sortBy compareUCB ch
          compareUCB = compare `on` (ucb d . getGameData)





