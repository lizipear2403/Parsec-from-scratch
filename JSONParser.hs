module JSONParser () where

import Helper
import StateMonad

-- union

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
    let x = ((JString . before '\"') cs, behind '\"' cs)
     in case c of
            '\"' -> Just x
            _ -> Nothing
newSepBy :: Char -> ParserT String Maybe a -> ParserT String Maybe [a]
newSepBy sp p = StateT $ \str ->
    sep' str (Just [])
  where
    sep' _ Nothing = Nothing
    sep' (c : cs) (Just jvs) =
        if c == sp
            then do
                (v, s') <- runStateT p cs
                sep' s' (Just (v : jvs))
            else Just (jvs, c : cs)

pair :: ParserT String Maybe (JValue, JValue)
pair = StateT $ \str -> do
    (k, (s : s')) <- runStateT quoteParser str
    case s of
        ':' -> do
            (v, s'') <- runStateT nullparse s'
            return $ ((k, v), s'')

between :: ParserT String Maybe () -> ParserT String Maybe a -> ParserT String Maybe () -> ParserT String Maybe a
between ps pa pe = StateT $ \str -> do
    (_, s) <- runStateT ps str
    (a, s') <- runStateT pa s
    (_, s'') <- runStateT pe s'
    return (a, s'')

-- lp = do
--
newNewSep :: ParserT String Maybe () -> ParserT String Maybe a -> ParserT String Maybe [a]
newNewSep sg p = StateT $ \str -> sep' str (Just [])
  where
    sep' s (Just jvs) = do
        runStateT sg s
            >>= \(_, s1) ->
                runStateT p s1
                    >>= \(v, s') -> sep' s' (Just (v : jvs))

consume :: Char -> ParserT String Maybe ()
consume s = StateT $
    \(c : str) -> if c == s then Just ((), str) else Nothing

defaultParser :: JParser
defaultParser = StateT $ \_ -> Nothing

ap = do
    consume '['
    xs <- newNewSep (consume ',') nullparse
    consume ']'
    return (JArray xs)

beforeP :: Char -> ParserT String Maybe a -> ParserT String Maybe a
beforeP c p = StateT $ \(ss) ->
    let (a, b) = break (== c) ss
     in do
            (v, s) <- runStateT p a
            return (v, b)

qp :: StateT String Maybe JValue
qp =
    between
        (consume '\"')
        (beforeP '\"' (StateT $ \s -> Just $ (JString s, [])))
        (consume '\"')

test = do
    s <- runStateT qp "abc"
    return $ fst s

--     consume '\"'
--     s <- newNewSep noneSep jstringParse
--     consume '\"'
--     return s
--         where
--             noneSep :: a ->  ParserT String Maybe ()
--             noneSep _ =  StateT $ \(c:str) -> Just ((), c:str)
--
op = do
    consume '{'
    xs <- newNewSep (consume ',') pair
    consume '}'
    return xs

-- between :: Char ->
-- ( Char -> ParserT (String -> Maybe (String , WHAT? )) ) ->
-- Char ->
-- ParserT ( String -> Maybe (String, JValue) )
-- 得到某种可以传递的东西
-- ParserT  可以传递
--
--
-- sepBy :: Char -> ParserT
-- between :
-- sepBy :
-- sepByPair :
-- arrayParser = ParserT (
--      between '[' ( sepBy ',' )  ']'
--      )
-- objectParser = parserT (between '{'  (sepByPair ':' ',' ) ''}')
genParser :: [JParser] -> JParser
genParser ps = foldl' (<&>) defaultParser ps

-- sep' :: Char -> Char -> ParserT String JValue -> String -> Maybe [JValue] -> Maybe [JValue]
-- sep' _ _ _ _ Nothing = Nothing
-- sep' s e p sr mvs = do
--     vs <- mvs
--     case sr of
--         h : rs
--             | h == s -> do
--                 (v, s1) <- runStateT p rs
--                 sep' s e p s1 (Just (vs ++ [v]))
--             | h == e -> Just vs
--         _ -> Nothing
--
-- 有待优化
-- 需要表达索引与否 换句话说 前一个值和后一个值通过什么方式互相影响
-- 并列-》 数组
-- 前一个是后一个的索引-》 对象的属性：值
-- 但是我们同样可以有：第一个变量是后面n-1个变量的索引
-- 如何抽象？
-- 分隔符/关联符 ： 决定下一个值以何种方式被关联
-- 并列：一元组 -> 数组
-- 两两成对 ->  二元组 -> 数组
-- 三三成对 -> 三元组
-- ......
-- 如何描述 -> HS不支持参数化的元组 无法写出元组作为利用
-- 使用 列表的列表 -> 并列 每一个列表len=1
-- 成对 -> len = 2
--
-- sep :: Char -> [ParserT String JValue] -> String -> Maybe  [ JValue ]
--
-- EndSym -> Maybe [JValue]
-- pairBy : 将值分组
-- sep -> 将字符串全部消费（注意递归性） 消费之后分组
-- 可行 只需要保持下降和消费
-- 但是无法解析混杂 二元组混杂一元组
--
-- sep -> 按分隔符分割
--
-- SepSym -> parse -> (a,s) -> a:vs -> sep s
--
--
--
--
--
-- parser : 起始符 终结符 关联性 关联符 元素解析器 谓词
-- 字符串解析 引号起始终结 没有关联性 元素解析器始终成立
-- 数组     中括号起始终结 无关联性 元素为字面量 数组
-- 对象     大括号起始终结 有关联性 关联符 冒号 元素解析器 索引必须为字符串 值为 数组 字面量 对象
numberParser :: JParser
numberParser = nullparse

boolParser :: JParser
boolParser = nullparse

-- p <- getLine
-- c <- readFile p
-- let a = preProcess c
-- let o = parse a
-- print o
