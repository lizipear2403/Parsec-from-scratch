module JSONParser (JValue (..), op) where

import Debug.Trace
import Helper
import ParserCombinator
import StateMonad

data JValue
    = JString String
    | JDouble Float
    | JInt Int
    | JBool Bool
    | JNull
    | JObject [(JValue, JValue)]
    | JArray [JValue]
    deriving (Eq, Show, Ord)

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
    xs <- sepBy (consume ',') qp
    consume ']'
    return (JArray xs)

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
op :: StateT String Maybe JValue
op = do
    consume '{'
    xs <- sepBy (consume ',') (pair ':' (qp, (qp <&> ap <&> op)))
    consume '}'
    return (JObject xs)

defaultParser :: JParser
defaultParser = StateT $ \_ -> Nothing

numberParser :: JParser
numberParser = nullparse

boolParser :: JParser
boolParser = nullparse
