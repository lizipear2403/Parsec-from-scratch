import Data.List (isInfixOf)
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
type JSONParser = Parser' String (Maybe JValue)

type JParser = ParserT String Maybe JValue

isDigit :: Char -> Bool
isDigit x = case x of
    '1' -> True
    '2' -> True
    '3' -> True
    '4' -> True
    '5' -> True
    '6' -> True
    '7' -> True
    '8' -> True
    '9' -> True
    '0' -> True
    '-' -> True
    _ -> False
before :: (Eq a) => a -> [a] -> [a]
before x = (fst . break (== x))

behind :: (Eq a) => a -> [a] -> [a]
behind x = drop 1 . snd . break (== x)

nullparse :: JParser
nullparse = StateT' $ const $ Just (JString "null", "remain")
quoteTest = map (runStateT quoteParser) ["\"abc", "\"abcd\"\""]

quoteParser :: JParser
quoteParser = StateT' $ \(c : cs) ->
    let x = ((JString . before '\"') cs, behind '\"' cs)
     in case c of
            '\"' -> Just x
            _ -> Nothing

defaultParser :: JParser
defaultParser = StateT' $ \_ -> Nothing

-- [x,y,z]
-- array 内部：字符串 数字 数组
-- 先找起始 终止符 按逗号分割
-- 辅助函数 ： splitOn
-- 对每个字符串进行内部解析
-- 并且解析结果：有一个失败全部失败：mapM ，
-- 成功条件：每个解析之后 只剩下空
--

-- array' = begin '['  >> sepBy parsers ',' -> xs  >> close ']'  >> JArray xs
-- how to know when to end
-- sepBy :: Char ->   ( String -> Maybe (String , JValue) ) ->  (String -> Maybe  (String , [JValue] ) )
-- --                  ^^^^^^^ParserT String Maybe JValue        ^^^^ParserT String Maybe  [JValue]
-- --
-- --do symbol '[' >> sepBy parser ',' >>= \ xs -> symbol ']' >> return xs
newSepBy :: Char -> ParserT String Maybe a -> ParserT String Maybe [a]
newSepBy sp p = StateT' $ \str ->
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
pair = StateT' $ \str -> do
    (k, (s : s')) <- runStateT quoteParser str
    case s of
        ':' -> do
            (v, s'') <- runStateT nullparse s'
            return $ ((k, v), s'')

ap = do
    consume '['
    xs <- newNewSep (consume ',') nullparse
    consume ']'
    return (JArray xs)

op = do
    consume '{'
    xs <- newNewSep (consume ',') pair
    consume '}'
    return xs

newNewSep :: ParserT String Maybe () -> ParserT String Maybe a -> ParserT String Maybe [a]
newNewSep sg p = StateT' $ \str -> sep' str (Just [])
  where
    sep' s (Just jvs) = do
        runStateT sg s
            >>= \(_, s1) ->
                runStateT p s1
                    >>= \(v, s') -> sep' s' (Just (v : jvs))

consume :: Char -> ParserT String Maybe ()
consume s = StateT' $
    \(c : str) -> if c == s then Just ((), str) else Nothing

--
-- 消费字符串有两种：有产出和无产出
-- 不必把产出类型放到解析器里面
-- 解析器可以只是String -> Maybe (String ,String )
--
-- consume :: Char -> ParserT String Maybe JValue
-- consume c = StateT' $ \str ->
--     if head str == c
--         then
--             Just (tail str,)
--         else Just (str,)
--
-- newarray = do
-- consume '['
-- xs <- newSepBy ',' parsers
-- consume ']'
--
-- ParserT String Maybe JValue >>
-- ParserT String Maybe [JValue] >>=
-- \xs ->
-- ParserT String Maybe  JValue
-- >> return $  JArray xs
--
--
--
-- sep' :: Char -> ParserT String Maybe JValue -> Maybe [JValue] -> ParserT String Maybe [JValue]
-- sep' _ _ Nothing = StateT' $ \_ -> Nothing
-- sep' spt p (Just jvs) = StateT' $ \(h:str) -> if  h == spt then  runStateT p str >>= \(s', v) -> sep' spt p (Just (v:jvs))

-- sep' s p  jvs =
-- \| h == s = do
--     (s', v) <- runStateT p str
--     sep' s p s' (v : jvs)
-- \| otherwise = (h : str, jvs)

test s = do
    xs <- runStateT arrayParser s
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

arrayTest =
    map
        (runStateT arrayParser)
        [ "[\"abc\"]"
        , "[[[[\"abc\"],\"abc\"],\"abc\"],\"abc\"]"
        ]
arrayParser :: JParser
arrayParser = StateT' $ \(c : cs) ->
    -- 需要重构：思路和对象解析相同 必须有状态递归 或者直接按逗号求
    case c of
        '[' -> ap (Just []) cs
        _ -> Nothing
  where
    parsers = foldl' (<&>) defaultParser [quoteParser, arrayParser]
    ap (Just jvs) [] = Just (JArray jvs, [])
    ap Nothing _ = Nothing
    -- close ']' = Just (res)
    -- sepBy = recursive
    ap (Just jvs) (x : xs) = case x of
        ']' -> Just (JArray jvs, xs)
        ',' -> ap (Just jvs) xs
        _ -> do
            (a, s) <- runStateT parsers (x : xs)
            ap (Just (a : jvs)) s
genParser ps = foldl' (<&>) defaultParser ps

sepBy :: Char -> Char -> [JParser] -> String -> Maybe [JValue]
sepBy sp ed ps s = sep1 s (Just [])
  where
    sep1 :: String -> Maybe [JValue] -> Maybe [JValue]
    sep1 s v = do
        vs <- v
        case s of
            h : rs | h == sp -> do
                (v, s1) <- runStateT (genParser ps) rs
                sep1 s1 (Just (vs ++ [v]))

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
objectTest =
    map
        (runStateT objectParser)
        [ "{\"abc\":{\"a\":\"a\"}}"
        , "{\"a\":\"b\",\"a\":[\"a\"]}"
        ]

objectParser :: JParser
objectParser = StateT' $ \cs -> case cs of
    ('{' : s) -> obp (Just []) s
    _ -> Nothing
  where
    parsers =
        quoteParser
            <&> objectParser
            <&> arrayParser
    -- <&> objectParser
    -- <&> arrayParser
    getString (JString s) = s
    obp :: Maybe [(String, JValue)] -> String -> Maybe (JValue, String)
    obp Nothing _ = Nothing
    obp (Just jos) (x : xs) = case x of
        '}' -> Just (JObject jos, xs)
        ',' -> obp (Just jos) (xs)
        _ -> do
            (q1', s1) <- runStateT quoteParser (x : xs)
            let q1 = getString q1'
            case s1 of
                ':' : s2 -> do
                    (q2, s3) <- runStateT parsers s2
                    obp (Just ((q1, q2) : jos)) s3
                _ -> Nothing

nullParser :: JParser
nullParser = StateT' $ \(c : cs) -> case c of
    'n' -> case cs of
        ('u' : cs') -> case cs' of
            ('l' : cs'') -> case cs'' of
                'l' : cs''' -> Just (JNull, cs''')
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing

numberParser :: JParser
numberParser = nullparse

boolParser :: JParser
boolParser = nullparse

-- parseQuoteMaybe :: JSONParser
-- parseQuoteMaybe = State' $ \(c : cs) -> case c of
--     '\"' -> wrapFst $ getAll parseQuote cs
--     _ -> (Nothing, c : cs)
--
-- parseArrayMaybe :: JSONParser
-- parseArrayMaybe = State' $ \(c : cs) -> case c of
--     '[' -> wrapFst $ getAll parseArray cs
--     _ -> (Nothing, (c : cs))
--
-- parseObjectMaybe :: JSONParser
-- parseObjectMaybe = State' $ \(c : cs) -> case c of
--     '{' -> wrapFst $ getAll parseObject cs
--     _ -> (Nothing, (c : cs))
-- parseLiteralMaybe :: JSONParser
-- parseLiteralMaybe = State' $ \(c : cs) -> case c of {}
-- getAll :: State' s a -> s -> (a, s)
-- getAll (State' sa) s = sa s

-- 预处理: 去除空行，空格， 缩进
-- test = evalState (parseArray) arrayTest

-- newParseArray :: State' String JValue
-- newParseArray = do

-- parseArrayn :: Parser' String JValue
-- parseArrayn = State' $ \(c : cs) -> parseArray' (c : cs) []
--   where
--     parseArray' :: State' String [JValue]
--     parseArray' (x : xs) jvs = case x of
--         '\"' ->
--             let (q, s) = (runState parseQuote) xs
--              in parseArray' (q : jvs) s
--         '{' ->
--             let (q, s) = (runState parseObject) xs
--              in parseArray' (q : jvs) s
--         '[' ->
--             let (q, s) = (runState parseArray) xs
--              in parseArray' (q : jvs) s
--         ']' -> (JArray jvs, xs)
--         ',' -> parseArray' jvs xs
--         _ ->
--             let (q, s) = (runState parseLiteral) xs
--              in parseArray' (q : jvs) s
--
-- 最基本的包裹：字符串， 双引号内部所有内容都被当作引用解释
-- 传入一个已经由引号开头的字符串，接下来找到第一个引号
-- 得到一个引用和一个剩余字符串
-- 解析字面量： 数字， 布尔
-- 暂缓
-- 数字： 小数点 负号 数字
--      数字解析：必须保存开始标志:
-- parseNumber :: String -> (JValue, String )
-- parseNumber (c:cs) = case c of -- c是数字
-- parseQuote :: Parser' String JValue
-- parseQuote = State' $ \s -> ((JString . before '\"') s, behind '\"' s)
--
-- parseArray :: Parser' String JValue
-- parseArray = State' $ \(c : cs) -> parseArray' [] (c : cs)
--   where
--     parseArray' jvs (x : xs) = case x of
--         '\"' -> let (q, s) = (runState parseQuote) xs in parseArray' (q : jvs) s
--         '{' -> let (q, s) = (runState parseObject) xs in parseArray' (q : jvs) s
--         '[' -> let (q, s) = (runState parseArray) xs in parseArray' (q : jvs) s
--         ']' -> (JArray jvs, xs)
--         ',' -> parseArray' jvs xs
--         _ -> let (q, s) = (runState parseLiteral) xs in parseArray' (q : jvs) s
--
-- -- parseObject :: Parser String JValue
-- parseObject :: Parser' String JValue
-- parseObject = State' $ \(c : cs) -> parseObject' [] (c : cs)
--   where
--     parseObject' jos (x : xs) = case x of
--         '\"' ->
--             let (q, s) = (runState parseQuote) xs
--              in case s !! 1 of
--                     '\"' -> let (q', s') = (runState parseQuote) (drop 2 s) in parseObject' ((q, q') : jos) s'
--                     '[' -> let (q', s') = (runState parseArray) (drop 2 s) in parseObject' ((q, q') : jos) s'
--                     '{' -> let (q', s') = (runState parseObject) (drop 2 s) in parseObject' ((q, q') : jos) s'
--                     _ -> let (q', s') = (runState parseLiteral) (drop 1 s) in parseObject' ((q, q') : jos) s'
--         ',' -> parseObject' jos xs
--         '}' ->
--             let jos' = map (\(a, b) -> (getString a, b)) jos
--                 getString (JString s) = s
--              in (JObject jos', xs)
--
-- parseNonLiteral :: Parser' String JValue
-- parseNonLiteral = State' $ \(c : cs) -> case c of
--     '\"' -> (runState parseQuote) cs
--     '[' -> (runState parseArray) cs
--     '{' -> (runState parseObject) cs
--     _ -> (runState parseLiteral) cs
--
-- parseNumber :: Parser' String JValue
-- parseNumber = State' $ \(c : cs) -> parseNumber' [c] (cs)
--   where
--     readNumber cs = if isInfixOf "." cs then JDouble ((read cs) :: Float) else (JInt . read) cs
--     parseNumber' ns (x : xs) = case x of
--         ',' -> (readNumber ns, xs)
--         _ -> parseNumber' (ns ++ [x]) xs
--
-- parseNull :: Parser' String JValue
-- parseNull = State' $ \(c : cs) -> parseNull' [c] cs
--   where
--     parseNull' ns (x : xs) = case x of
--         'u' -> parseNull' (ns ++ [x]) xs
--         'l' -> parseNull' (ns ++ [x]) xs
--         ',' -> (readNull ns, xs)
--           where
--             readNull _ = JNull
--
-- -- false
-- -- true
--
-- parseBool :: Parser' String JValue
-- parseBool = State' $ \(c : cs) -> parseBool' [c] cs
--   where
--     parseBool' ns (x : xs) = if inBool x then parseBool' (ns ++ [x]) xs else ((readBool ns), xs)
--       where
--         inBool c = elem c "falsetrue"
--         readBool cs =
--             if cs == "false"
--                 then
--                     JBool False
--                 else JBool True
--
-- parseLiteral :: Parser' String JValue
-- parseLiteral = State' $ \(c : cs) ->
--     if isDigit c
--         then (runState parseNumber) (c : cs)
--         else
--             if c == 'n'
--                 then (runState parseNull) (c : cs)
--                 else
--                     if c == 't' || c == 'f'
--                         then (runState parseBool) (c : cs)
--                         else
--                             (runState parseNonLiteral) (c : cs)
-- objectTestNew = "{\"a\":\"b\",\"a\":true,\"a\":[\"c\"],\"a\":{\"c\":[\"a\"]}}"
--
-- parse :: String -> JValue
-- parse = fst . (runState parseObject) . (drop 1)

main :: IO ()
main = putStrLn "Hello Parser World"

-- p <- getLine
-- c <- readFile p
-- let a = preProcess c
-- let o = parse a
-- print o
