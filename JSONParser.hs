module JSONParser (JValue (..), objectParser) where

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

stringParser =
    between
        (consume '\"')
        ( beforeWith' (/= '\"') (const False) (\s -> (JString s, ""))
        -- '\"'
        -- (StateT $ \s -> Just $ (JString s, []))
        )
        (consume '\"')

-- stringParser =
--     between
--         (consume '\"')
--         ( ParserCombinator.before
--             '\"'
--             (StateT $ \s -> Just $ (JString s, []))
--         )
--         (consume '\"')
arrayParser = do
    consume '['
    xs <-
        sepBy
            (consume ',')
            ( genParser
                [ stringParser
                , arrayParser
                , objectParser
                , boolParser
                , numberParser
                ]
                defaultParser
            )
    consume ']'
    return (JArray xs)

objectParser :: StateT String Maybe JValue
objectParser = do
    consume '{'
    xs <-
        sepBy
            (consume ',')
            ( pair
                ':'
                ( stringParser
                , genParser
                    [ nullParser
                    , stringParser
                    , arrayParser
                    , boolParser
                    , numberParser
                    , objectParser
                    ]
                    defaultParser
                )
            )
    consume '}'
    return (JObject xs)

defaultParser :: JParser
defaultParser = StateT $ \_ -> Nothing

numberParser :: JParser
numberParser = beforeWith' isDigit (== "") (\s -> (JFloat ((read s) :: Float), ""))

-- beforeWith
--     (isDigit)
--     ( StateT $
--         \str -> case str of
--             "" -> Nothing
--             _ -> Just (JFloat ((read str) :: Float), "")
--     )

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
