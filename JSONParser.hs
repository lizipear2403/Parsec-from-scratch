module JSONParser (JValue (..), op) where

import Debug.Trace
import Helper
import ParserCombinator
import StateMonad

data JValue
    = JString String
    | JFloat Float
    | JBool Bool
    | JNull
    | JObject [(JValue, JValue)]
    | JArray [JValue]
    deriving (Eq, Show, Ord)

type JParser = ParserT String Maybe JValue

nullparse :: JParser
nullparse = StateT $ const $ Just (JString "null", "remain")

quoteParser :: JParser
quoteParser = StateT $ \(c : cs) ->
    let x = ((JString . Helper.before '\"') cs, behind '\"' cs)
     in case c of
            '\"' -> Just x
            _ -> Nothing
qp =
    between
        (consume '\"')
        (ParserCombinator.before '\"' (StateT $ \s -> Just $ (JString s, [])))
        (consume '\"')
ap = do
    consume '['
    xs <- sepBy (consume ',') (qp <&> ap <&> op <&> boolParser <&> numberParser)
    consume ']'
    return (JArray xs)

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
    xs <- sepBy (consume ',') (pair ':' (qp, nullParser <&> qp <&> ap <&> boolParser <&> op <&> numberParser))
    consume '}'
    return (JObject xs)

defaultParser :: JParser
defaultParser = StateT $ \_ -> Nothing

-- 只有放在最后才不会出bug？
numberParser :: JParser
numberParser =
    beforeWith
        (not . isDigit)
        ( StateT $
            \str -> Just (JFloat ((read str) :: Float), "")
        )

nullParser :: JParser
nullParser = do
    consume 'n'
    consume 'u'
    consume 'l'
    consume 'l'
    return JNull

boolParser :: JParser
boolParser = trueParser <&> falseParser

falseParser :: JParser
falseParser = do
    consume 'f'
    consume 'a'
    consume 'l'
    consume 's'
    consume 'e'
    return (JBool False)

trueParser :: JParser
trueParser = do
    consume 't'
    consume 'r'
    consume 'u'
    consume 'e'
    return (JBool True)
