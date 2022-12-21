module Main (main) where

import MCTS
import System.Environment(getArgs)
import Data.Matrix
import System.Random
import Data.Vector(findIndex)

---------------
-- FOR TESTING
---------------

data SimpleState = Yes | No deriving (Show, Eq)

instance GameState SimpleState where
    next Yes = [Yes, No]
    next No  = [Yes]
    eval Yes = One
    eval No = Two
    pick _ gs = head gs
    sim = eval

testStep :: (Show g, GameState g) => Int -> Player -> g -> IO ()
testStep n p g = (putStrLn . drawGameTree) (applyNtimes n step r)
    where r = root p g

testMCTS :: (Show g, GameState g) => Int -> Player -> g -> IO ()
testMCTS n p g = print $ mcts n p g

----------------------------
-- CONNECT 4 IMPLEMENTATION
----------------------------

maxRun :: Ord a => [a] -> Int
maxRun []  = 0
maxRun lst@(h:_) = length run `max` maxRun rest
            where (run, rest) = span (== h) lst 

data Color = Red | Yellow | Blank deriving (Show, Eq)

opposite :: Color -> Color
opposite Red    = Yellow
opposite Yellow = Red
opposite Blank  = Blank

toPlayer :: Color -> Player
toPlayer Red    = One
toPlayer Yellow = Two
toPlayer Blank  = Tie --garbage

data (RandomGen r, Show r) => ConnectFourState r = State {board :: Matrix Color, currentPlayer :: Color, lastMove :: (Int, Int), rng :: r} deriving (Show) 

initial :: Int -> ConnectFourState StdGen
initial seed = State (matrix 6 7 $ \_ -> Blank) Red (-1,-1) (mkStdGen seed)

instance (RandomGen r, Show r) => GameState (ConnectFourState r) where 
    next (State b p _ r) = map (\(move,childBoard) -> State childBoard nextPlayer move newRNG) boards
        where 
              nextPlayer = opposite p
              newRNG = snd $ split r

              boards = zip indeces $ map (\(i,j) -> setElem p (i,j) b) indeces

              indeces = zip rowIndeces colIndeces

              rowIndeces = map (getRowIndex . findIndex (/= Blank) . \j -> getCol j b) colIndeces --get colummns corresponding to indeces then find index of first row that isn't blank 
              colIndeces = filter (\j -> (getElem 1 j b) == Blank) [1..ncols b] --get column indeces where there is space]

              getRowIndex (Just i) = i - 1
              getRowIndex Nothing = nrows b 

    -- eval (State b p l _) = 
    --     where 
              

    --           sndDiag = [getElem (r+i) (c+j) b | (i, j) <- zip [-7..7] [7,6..(-7)], 1 <= r+i && r+i <= 6 && 1 <= c+j && c+j <= 7]
    --           fstDiag = [getElem (r+i) (c+j) b | (i, j) <- zip [-7..7] [-7..7], 1 <= r+i && r+i <= 6 && 1 <= c+j && c+j <= 7]
    --           row = toList . getRow r b
    --           col = toList . getCol c b
    --           (r, c) = l

    pick (State _ _ _ r) states = states !! i
        where (i, _) = uniformR (0 :: Int, length states - 1) r



----------------------------
-- ENTRY POINT
----------------------------

main :: IO ()
main = do
    args <- getArgs
    let arg1 = head args
    let n = (read arg1) :: Int
    let nextState = mcts n Two Yes
    testStep n One nextState
    testMCTS n One nextState
