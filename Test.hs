module Test (test, dirPath) where

import JSONParser
import StateMonad
import System.Directory

-- 不充分的测试用例
--
-- test dir -> all test file -> [ test content ] -> test
type Path = String
dirPath :: String
dirPath = "./Test/"

pp :: String -> String
pp str = filter (not . (\c -> c == ' ' || c == '\n')) str

-- [c | c <- str, c /= ' ' && c /= '\n']

getAllTestCont :: Path -> IO [String]
getAllTestCont p = do
    paths <- listDirectory p
    c <- mapM readFile (map (dirPath ++) paths)
    let res = map pp c
    return res

collectAll :: [String] -> [Maybe JValue]
collectAll cont = map (parsec) cont
  where
    parsec :: String -> Maybe JValue
    parsec s = do
        xs <- runStateT objectParser s
        return $ fst xs

test :: Path -> IO [Maybe JValue]
test p = do
    content <- getAllTestCont p
    let res = collectAll content
    return res
