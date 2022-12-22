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

    putStrLn $ "MCTS SIMULATION: CONNECT 4"    
    simulation n rollout (initial seed) 1
