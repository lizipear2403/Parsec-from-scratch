module Main where

import Test

main :: IO ()
main = do
    ls <- test dirPath
    print ls
    putStrLn "Hello Parser World !"
