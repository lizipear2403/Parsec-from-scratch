import Debug.Trace
import Helper
import ParserCombinator
import StateMonad

data Value = Pair (Value, Value) | Value String
    deriving (Show)

isValue :: Char -> Bool
isValue c = c /= ',' && c /= '(' && c /= ')'
sp' = beforeWith' isValue (== "") (\s -> (Value s, ""))

parseTuple :: StateT String Maybe (Value)
parseTuple = do
    consume '('
    (a, b) <- pair ',' (sp' <&> parseTuple, sp' <&> parseTuple)
    consume ')'
    return $ Pair (a, b)
