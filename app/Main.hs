module Main (main) where

import MCTS

---------------
-- FOR TESTING
---------------

data SimpleState = Yes | No deriving (Show, Eq)

instance GameState SimpleState where
    next Yes = [Yes, No]
    next No  = [Yes]
    eval Yes = One
    eval No = Two
    pick = head
    sim = eval

testStep :: (Show g, GameState g) => Int -> g -> IO ()
testStep n g = mapM_ (putStrLn . drawGameTree) (take n $ iterate step r)
    where r = root g

testMCTS :: (Show g, GameState g) => Int -> g -> IO ()
testMCTS n g = print $ mcts n g

main :: IO ()
main = testStep 7 Yes
