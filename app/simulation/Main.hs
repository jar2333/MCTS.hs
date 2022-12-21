module Main (main) where

import ConnectFour

main :: IO ()
main = simulation 1500 7 (initial 139) 1
