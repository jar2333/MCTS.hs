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

data GameData g = GameData {wins :: Int, total :: Int, player :: Player, game_state :: g}

type GameTree g = Tree (GameData g)

getGameData :: GameTree g -> GameData g
getGameData (Node game_data _) = game_data 

mcts :: GameState g => GameState g => g -> g
mcts s = s

step :: GameState g => GameTree g -> GameTree g
step t = t

ucb :: GameState g => GameData g -> GameData g -> Double
ucb GameData{total=p_total} GameData{wins=wins, total=c_total} = (w / n) + c * sqrt (log np / n)
    where n  = fromIntegral (c_total + 1) -- to avoid division by 0?
          np = fromIntegral p_total
          w  = fromIntegral wins
          c = sqrt (2.0 :: Double)

-- Returns a list of children tree nodes created from given node's game state
expand :: GameState g => GameTree g -> [GameTree g]
expand (Node d@GameData{game_state=g, player=p} ch) = ch++newChildren
    where 
          newChildren = map newChild possibleMoves
          newChild m = Node GameData{wins=0, total=0, game_state=m, player=childPlayer} []
          childPlayer = opposite p
          possibleMoves = take 2 $ next g

-- Takes a tree, traverses to a leaf using UCB, then expands it
-- Use the state monad to propagate upwards the (results of simulation, winning player) as a state
-- The expansion 
walk :: GameState g => GameTree g -> GameTree g
walk n@(Node d []) = Node d $ expand n
walk n@(Node d ch) = Node d $ (walk selected):rest
    where 
          selected:rest = sortBy compareUCB ch
          compareUCB = compare `on` (ucb d . getGameData)

-- select :: GameState g => GameTree g -> GameNode g
-- select 




