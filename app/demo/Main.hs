module Main (main) where

import ConnectFour
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    let arg1:arg2:arg3:_ = args
    let n = (read arg1) :: Int
    let rollout = (read arg2) :: Int

    let seed = (read arg3) :: Int
    let initialState = initial seed

    putStrLn $ "MCTS DEMO: CONNECT 4"
    putStrLn $ "Warning: no bounds checking on user input yet please be kind to me."
    game n rollout initialState 1



