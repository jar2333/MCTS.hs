module ConnectFour (
    game,
    initial
) where

import MCTS
import Data.Matrix
import System.Random
import Data.Vector(findIndex, toList)

----------------------------
-- CONNECT 4 IMPLEMENTATION
----------------------------
data Color = Red | Yellow | Blank deriving (Eq)

instance Show Color where
    show Red = "R"
    show Yellow = "Y"
    show Blank = "."

other :: Color -> Color
other Red    = Yellow
other Yellow = Red
other Blank  = Blank

toPlayer :: Color -> Player
toPlayer Red    = One
toPlayer Yellow = Two

------------------------------------------
-- Game specific helpers
------------------------------------------
place :: Matrix Color -> Int -> Int
place board = getRowIndex . findIndex (/= Blank) . \j -> getCol j board
    where getRowIndex (Just i) = i
          getRowIndex Nothing = nrows board 

maxRun :: [Color] -> Int
maxRun []  = 0
maxRun lst@(h:_) = (case h of 
                        Blank -> 0 
                        _     -> length run) `max` maxRun rest
            where (run, rest) = span (== h) lst 

------------------------------------------
-- MCTS GameState instance definitions!
------------------------------------------
data (RandomGen r) => ConnectFourState r = State {board :: Matrix Color, currentPlayer :: Color, lastMove :: (Int, Int), rng :: r}

instance (RandomGen r) => Show (ConnectFourState r) where
    show (State b p l _) = "\n" ++ show b ++ "\ncurrent player: " ++ show p ++ "\nlast move: " ++ show l

instance (RandomGen r) => GameState (ConnectFourState r) where 
    next (State b p _ r) = map (\(move,childBoard) -> State childBoard nextPlayer move newRNG) boards
        where 
              nextPlayer = other p
              newRNG = snd $ split r

              boards = zip indeces $ map (\(i,j) -> setElem p (i,j) b) indeces

              indeces = zip rowIndeces colIndeces

              rowIndeces = map (place b) colIndeces --get colummns corresponding to indeces then find index of first row that isn't blank 
              colIndeces = filter (\j -> (getElem 1 j b) == Blank) [1..ncols b] --get column indeces where there is space]

    eval (State b p l _) = if isRun 
                               then Just(toPlayer $ other p) --if a run is encountered, last move completed it, hence last player won
                               else Nothing
        where 
              isRun = maxRun sndDiag >= 4 || maxRun fstDiag >= 4 || maxRun row >= 4 || maxRun col >= 4

              sndDiag = [getElem (r+i) (c+j) b | (i, j) <- zip [-7..7] [7,6..(-7)], 1 <= r+i && r+i <= 6 && 1 <= c+j && c+j <= 7]
              fstDiag = [getElem (r+i) (c+j) b | (i, j) <- zip [-7..7] [-7..7], 1 <= r+i && r+i <= 6 && 1 <= c+j && c+j <= 7]
              row = Data.Vector.toList $ getRow r b
              col = Data.Vector.toList $ getCol c b

              (r, c) = l

    pick (State _ _ _ r) states = states !! i
        where (i, _) = uniformR (0 :: Int, length states - 1) r

initial :: Int -> ConnectFourState StdGen
initial seed = State (matrix 6 7 $ \_ -> Blank) Red (-1,-1) (mkStdGen seed)

----------------------------
-- ENTRY POINT
----------------------------
game :: Int -> Int -> ConnectFourState StdGen -> Int -> IO ()
game n rollout s@(State b _ _ r) turn = do
    putStrLn $ "Turn " ++ show turn ++ ":\nPlayer's turn (choose a column): " ++ show s ++ "\n"
    j <- readLn :: IO Int
    let i = place b j
    let playerState = State (setElem Red (i,j) b) Yellow (i,j) r
    putStrLn $ show playerState ++ "\n"

    -- win check
    case eval playerState of
        Just(One) -> putStrLn "Player Win!"
        _ -> do
                putStrLn $ "Turn " ++ show (turn+1) ++ ":\nComputer's turn: \n"
                let newState = mcts n rollout Two playerState
                putStrLn $ show newState ++ "\n"

                -- win check
                case eval newState of
                    Just(Two) -> putStrLn "Computer Win!"
                    _ -> game n rollout newState (turn+2)