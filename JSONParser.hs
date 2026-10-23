module JSONParser () where

import Helper
import ParserCombinator
import StateMonad

data JValue
    = JString String
    | JDouble Float
    | JInt Int
    | JBool Bool
    | JNull
    | JObject [(String, JValue)]
    | JArray [JValue]
    deriving (Eq, Show, Ord)

type JSONParser = Parser String (Maybe JValue)

type JParser = ParserT String Maybe JValue

nullparse :: JParser
nullparse = StateT $ const $ Just (JString "null", "remain")
quoteTest = map (runStateT quoteParser) ["\"abc", "\"abcd\"\""]

quoteParser :: JParser
quoteParser = StateT $ \(c : cs) ->
    let x = ((JString . Helper.before '\"') cs, behind '\"' cs)
     in case c of
            '\"' -> Just x
            _ -> Nothing
ap = do
    consume '['
    xs <- sepBy (consume ',') nullparse
    consume ']'
    return (JArray xs)
qp :: ParserT String Maybe JValue
qp =
    between
        (consume '\"')
        (ParserCombinator.before '\"' (StateT $ \s -> Just $ (JString s, [])))
        (consume '\"')

test = do
    s <- runStateT qp "abc"
    return $ fst s

--     consume '\"'
--     s <- sepBy noneSep jstringParse
--     consume '\"'
--     return s
--         where
--             noneSep :: a ->  ParserT String Maybe ()
--             noneSep _ =  StateT $ \(c:str) -> Just ((), c:str)
--
op = do
    consume '{'
    xs <- sepBy (consume ',') (pair ':' (nullparse, nullparse))
    consume '}'
    return xs

defaultParser :: JParser
defaultParser = StateT $ \_ -> Nothing

numberParser :: JParser
numberParser = nullparse

boolParser :: JParser
boolParser = nullparse
