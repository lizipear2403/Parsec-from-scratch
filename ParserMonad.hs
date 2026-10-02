import Data.List (isInfixOf)
import Debug.Trace

data JValue
    = JString String
    | JDouble Float
    | JInt Int
    | JBool Bool
    | JNull
    | JObject [(String, JValue)]
    | JArray [JValue]
    deriving (Eq, Show, Ord)
data StateT' s m a = StateT'
    { runStateT :: s -> m (a, s)
    }

data Identity a = Identity
    { runIdentity :: a
    }

data State' s a = State'
    { runState :: s -> (a, s)
    }

type Parser' a b = State' a b

type JSONParser = Parser' String (Maybe JValue)

type ParserT s a = StateT' s Maybe a

-- ParserMaybe :: String -> Maybe (JValue, String)

class (Applicative f) => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    (<\>) :: (b -> f a) -> (b -> f a) -> b -> f a
    (<\>) f1 f2 b = liftA2 (<|>) f1 f2 b

-- t s = monad (a,s) -> monad (b, s )
--
-- Identity a ::  a -> a
--

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    liftA2 abc (Identity a) (Identity b) = Identity (abc a b)

instance Monad Identity where
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (Identity a) >>= aIb = aIb a

instance (Monad m) => Functor (StateT' s m) where
    -- s -> m (a,s) -> s -> m (b,s)
    -- s-> m (a,s) >>= \(a,s') -> return ( f a , s')
    fmap :: (Monad m) => (a -> b) -> StateT' s m a -> StateT' s m b
    fmap f (StateT' t) = StateT' $ \s -> do
        (a, s') <- t s
        return (f a, s')

instance Functor (State' s) where
    fmap :: (a -> b) -> State' s a -> State' s b
    fmap f (State' t) = State' $ \s -> let (v, s') = t s in (f v, s')

instance Applicative (State' s) where
    pure :: a -> State' s a
    pure x = State' $ \s -> (x, s)
    liftA2 :: (a -> b -> c) -> State' s a -> State' s b -> State' s c
    liftA2 abc (State' sa) (State' sb) = State' $ \s ->
        let
            (a, s') = sa s
            (b, s'') = sb s'
         in
            (abc a b, s'')

instance (Monad m) => Applicative (StateT' s m) where
    pure :: (Monad m) => a -> StateT' s m a
    pure x = StateT' $ \s -> return (x, s)
    liftA2 :: (Monad m) => (a -> b -> c) -> StateT' s m a -> StateT' s m b -> StateT' s m c
    liftA2 abc (StateT' sma) (StateT' smb) = StateT' $ \s ->
        sma s
            >>= \(a, s') ->
                smb s'
                    >>= \(b, s'') -> return (abc a b, s'')

instance (Monad m) => Monad (StateT' s m) where
    (>>=) :: (Monad m) => StateT' s m a -> (a -> StateT' s m b) -> StateT' s m b
    (StateT' sma) >>= asmb = StateT' $ \s ->
        sma s >>= \(a, s') ->
            let StateT' smb = asmb a
             in smb s'

instance Alternative Maybe where
    empty = Nothing
    Nothing <|> f = f
    (Just x) <|> _ = Just x

instance Monad (State' s) where
    (>>=) :: State' s a -> (a -> State' s b) -> State' s b
    (>>=) (State' sa) asb = State' $ \s ->
        let
            (a, s') = sa s
            (State' sb) = asb a
         in
            sb s'
nullparse :: ParserT String JValue
nullparse = StateT' $ \_ -> Just (JString "null", "remain")
quoteTest = map (runStateT quoteParser) ["\"abc", "\"abcd\"\""]

quoteParser :: ParserT String JValue
quoteParser = StateT' $ \(c : cs) ->
    let x = ((JString . before '\"') cs, behind '\"' cs)
     in case c of
            '\"' -> Just x
            _ -> Nothing

(<&>) :: (Alternative m) => StateT' s m a -> StateT' s m a -> StateT' s m a
(StateT' sma1) <&> (StateT' sma2) = StateT' (sma1 <\> sma2)

defaultParser :: ParserT String JValue
defaultParser = StateT' $ \_ -> Nothing

-- [x,y,z]
-- array 内部：字符串 数字 数组
-- 先找起始 终止符 按逗号分割
-- 辅助函数 ： splitOn
-- 对每个字符串进行内部解析
-- 并且解析结果：有一个失败全部失败：mapM ，
-- 成功条件：每个解析之后 只剩下空
arrayTest =
    map
        (runStateT arrayParser)
        [ "[\"abc\"]"
        , "[[[[\"abc\"],\"abc\"],\"abc\"],\"abc\"]"
        ]
arrayParser :: ParserT String JValue
arrayParser = StateT' $ \(c : cs) ->
    -- 需要重构：思路和对象解析相同 必须有状态递归 或者直接按逗号求
    case c of
        '[' -> ap (Just []) cs
        _ -> Nothing
  where
    parsers = foldl' (<&>) defaultParser [quoteParser, arrayParser]
    ap (Just jvs) [] = Just (JArray jvs, [])
    ap Nothing _ = Nothing
    ap (Just jvs) (x : xs) = case x of
        ']' -> Just (JArray jvs, xs)
        ',' -> ap (Just jvs) xs
        _ -> do
            (a, s) <- runStateT parsers (x : xs)
            ap (Just (a : jvs)) s

-- data Token a = Single Char | Many String | Value a
--     deriving (Show, Eq)
--
-- sep :: Char -> String -> [ParserT String JValue] -> Maybe [[ Token JValue ]]
-- sep c s ps =  let parser = foldl' (<&>) defaultParser ps
--                 in sep' c s parser (Just [])
--                where
--                 sep' :: Char -> String -> ParserT String JValue -> Maybe [[ Token JValue ]] -> Maybe [[ Token JValue ]]
--                 sep' sp sr p tks = do
--                     tk <- tks
--                     case sr of
--                       h : rs -> if h == sp then do
--                                            (v,s1) <- runStateT  p rs
--                                            let new = tks ++ [Value v]
--                                            sep' sp sr p new
--                                            else do
--                                                let c1 = Single h
--                                                let last = last tks
--
--
--                           _ -> Nothing
--

genParser ps = foldl' (<&>) defaultParser ps

sepBy :: Char -> Char -> [ParserT String JValue] -> String -> Maybe [JValue]
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
--
--
--
-- sepBy :: (Char, Char) -> [ParserT String JValue] -> String -> Maybe [[JValue]]
-- sepBy (_, _) _ [] = Nothing
-- sepBy (sp, ed) ps s =
--     let parsers = foldl' (<&>) defaultParser ps
--      in sep' (sp, ed) parsers (Just []) s
--   where
--     sep' :: (Char, Char) -> ParserT String JValue -> Maybe [[JValue]] -> String -> Maybe [[JValue]]
--     sep' (sp, eq) par vs s = do
--         ivs <- vs
--         case s of
--             a : s2 ->
--                 if a == eq
--                     then vs
--                     else do
--                         (v, s1) <- runStateT par s
--                         sep' (sp, eq) par (Just $ [v] : ivs) s1

-- sep :: Char -> Char -> String -> [ParserT String JValue] -> Maybe ([JValue], String)
-- sep e s str ps = sep' e s str ps (Just [])
--   where
--     sep' :: Char -> Char -> String -> [ParserT String JValue] -> Maybe [JValue] -> Maybe ([JValue], String)
--     sep' e1 s1 str1 ps1 (Just jvs) =
--         let parsers = foldl' (<&>) defaultParser ps1
--          in case str1 of
--                 (s : s') ->
--                     if s == e
--                         then Just (jvs, s')
--                         else
--                             if s == s1
--                                 then sep' e1 s1 s' ps1 (Just jvs)
--                                 else do
--                                     (a, s'') <- runStateT parsers s'
--                                     sep' e1 s1 s'' ps1 (Just (a : jvs))

-- e : str' -> Just ((jvs), str')
-- s1 : s2 -> sep' e1 s1 s2 ps1 (Just jvs)
-- _ -> do
--     (a, s) <- runStateT parsers str1
--     sep' e1 s1 s ps1 (Just (a : jvs))

-- sepBy  接收 ： 终止符 分隔符 得到 值列表

-- parser :: Char -> Char -> Bool -> Char -> ParserT String JValue
-- parser i e False _

-- parser : 起始符 终结符 关联性 关联符 元素解析器 谓词
-- 字符串解析 引号起始终结 没有关联性 元素解析器始终成立
-- 数组     中括号起始终结 无关联性 元素为字面量 数组
-- 对象     大括号起始终结 有关联性 关联符 冒号 元素解析器 索引必须为字符串 值为 数组 字面量 对象

-- runStateT quoteParser (x:xs)
-- >>= \(a,s') ->

-- let parsers = quoteParser
--     -- <&> arrayParser
--     -- <&> numberParser
--     -- <&> nullParser
--     ap ::
--         Maybe [JValue] ->
--         String ->
--         Maybe (JValue, String)
--     ap Nothing _ = Nothing
--     ap (Just jvs) [] = Just (JArray jvs, [])
--     ap (Just jvs) (x : xs) = case x of
--         -- (q1, s1) <- runStateT arrayParser xs
--         -- ap (Just (q1 : jvs)) s1
--         ']' -> Just (JArray $ jvs, xs)
--         ',' -> ap (Just $ jvs) xs
--         _ -> do
--             (q1, s1) <- runStateT parsers xs
--             ap (Just (q1 : jvs)) s1
--  in ap (Just []) cs
-- mapM need (splitOn (before ']' cs) ',') >>= \ls -> Just (JArray ls, before ']' cs)

-- objectParser :: ParserT String JValue
-- objectParser = StateT' $ \cs -> case cs of
--     ('{' : s) -> obp (Just []) s
--     _ -> Nothing
--   where
--     parsers =
--         quoteParser
--             <&> objectParser
--     -- <&> arrayParser
--     -- <&> objectParser
--     -- <&> arrayParser
--     getString (JString s) = s
--     obp :: Maybe [(String, JValue)] -> String -> Maybe (JValue, String)
--     obp Nothing _ = Nothing
--     obp (Just jos) (x : xs) = case x of
--         '}' -> Just (JObject jos, xs)
--         ',' -> obp (Just jos) (xs)
--         _ -> do
--             (q1', s1) <- runStateT quoteParser (x : xs)
--             let q1 = getString q1'
--             case s1 of
--                 ':' : s2 -> do
--                     (q2, s3) <- runStateT parsers s2
--                     obp (Just ((q1, q2) : jos)) s3
--                 _ -> Nothing
--
-- mapM -> Maybe [JValue]
-- need :: String -> Maybe JValue
-- parser :: String -> Maybe (JValue , String)
-- need = \s -> case (runStateT parser) s of
--      (Just (a,_)) = Just a
--      Nothing -> Nothing

-- "abc","abcde",[
-- where
--     parser =

--
-- arrayParser = StateT' $ \(c:cs) ->

objectTest =
    map
        (runStateT objectParser)
        [ "{\"abc\":{\"a\":\"a\"}}"
        , "{\"a\":\"b\",\"a\":[\"a\"]}"
        ]

objectParser :: ParserT String JValue
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

-- runStateT quoteParser (x : xs)
--     >>= \(jq, s') -> case s' of
--         ':' : s1 ->
--             runStateT parsers s1
--                 >>= \(jq2, s2) -> obp (Just ((jq, jq2) : jos)) s2

-- case x of
-- runStateT quoteParser xs >>= \(jq, s1) -> case s1 of
--     (':' : s1') -> runStateT parsers s1' >>= \(jv, s2) -> obp (Just ((jq, jv) : jos)) s2
--     (',':s1') ->
--     _ -> Nothing
-- '}' -> Just (JObject (map (\(jq, jv) -> (getString jq, jv)) jos), xs)
-- _ -> Nothing

-- StateT' $ \(c : cs) ->
-- let parsers =
--         quoteParser
--             <&> numberParser
--             <&> arrayParser
--             <&> objectParser
--     obp ::Maybe [(JValue, JValue)] -> String -> Maybe [(JValue, JValue)]
--     obp Nothing _ _ = Nothing
--     obp (Just os) (x:xs) = runStateT parseQuote (x:xs) >>=
--         \(q,s') -> case s' of
--                      ':': cs' -> runStateT parsers cs' >>= \(v,s'')
--                                         -> obp (Just ((q,v):os) ) s''
--                      ',':cs -> obp (Just os) cs
--                      ''
--  in case c of
--         '{' -> obp [] cs
--         _ -> Nothing
--
-- objectParser = StateT' $ \(c : cs) -> case c of
--     '{' ->
--         runStateT quoteParser cs
--             >>= \(a, s') ->
--                 let parsers =
--                         arrayParser
--                             <&> numberParser
--                             <&> quoteParser
--                             <&> nullParser
--                             <&> objectParser
--                  in objectParser'
--     _ -> Nothing
nullParser :: ParserT String JValue
nullParser = StateT' $ \(c : cs) -> case c of
    'n' -> case cs of
        ('u' : cs') -> case cs' of
            ('l' : cs'') -> case cs'' of
                'l' : cs''' -> Just (JNull, cs''')
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing

numberParser :: ParserT String JValue
numberParser = nullparse

boolParser :: ParserT String JValue
boolParser = nullparse

-- s = runStateT quoteParser "\"abc\""

parseQuoteMaybe :: JSONParser
parseQuoteMaybe = State' $ \(c : cs) -> case c of
    '\"' -> wrapFst $ getAll parseQuote cs
    _ -> (Nothing, c : cs)

parseArrayMaybe :: JSONParser
parseArrayMaybe = State' $ \(c : cs) -> case c of
    '[' -> wrapFst $ getAll parseArray cs
    _ -> (Nothing, (c : cs))

parseObjectMaybe :: JSONParser
parseObjectMaybe = State' $ \(c : cs) -> case c of
    '{' -> wrapFst $ getAll parseObject cs
    _ -> (Nothing, (c : cs))

-- 结构： 接收一个字符串 接收一个解析器 解析成功
-- 返回结果和改变的字符串
-- 失败：Nothing 字符串不动
-- parser的本质是 起始符号 终止符号 ( 可能 值 )
-- parseMaybe :: Char -> Char  -> Parser' String JValue -> JSONParser
-- parseMaybe s e p =

parseLiteralMaybe :: JSONParser
parseLiteralMaybe = State' $ \(c : cs) -> case c of {}

-- modify :: State' s a -> (s -> s ) -> State' s a
-- modify

-- Parser Literal , Parser Object , Parser Array
-- String -> check start -> parse or not
-- if not parse  -> Nothing -> try annother
-- only 3 parser -> try fst | snd | thr
-- a func : p1 p2 if p1 then p1 else if p2 then p2 else Nothing
-- => p1 -> p2 -> p3 -> Failed !!!
-- p1
-- 每个解析器：字符匹配-》解析 不匹配 -》 失败
-- 类型 String -> Maybe (State' String JValue)
-- StateT
-- Parser State -> Parser State
--
test1 :: Int -> Maybe Int
test1 x = if x > 1 then Just 1 else Nothing

test2 :: Int -> Maybe Int
test2 x = if x < -2 then Just (-1) else Nothing

testAll = test1 <\> test2 <\> test1

--
-- class (Applicative f) => Alternative f where
--     (<|>) :: f a -> f a -> f a
-- instance Alternative Maybe where
--     (<|>) :: Maybe a -> Maybe a -> Maybe a
--     Nothing <|> f1 = f1
--     l <|> _ = l
--
-- instance Alternative ((->) r) where
--     (<|>) :: (r -> a) -> (r -> a) -> (r -> a)
--     ar1 <|> ar2 = ar1
--
-- test1 :: Int -> Maybe Int
-- test1 x = if x > 5 then Just 1 else Nothing
-- test2 :: Int -> Maybe Int
-- test2 x = if x < 0 then Just (-1) else Nothing
--
-- --
-- testAll a = (test1 <|> test2) a
--
wrapFst :: (a, b) -> (Maybe a, b)
wrapFst (a, b) = (Just a, b)

getAll :: State' s a -> s -> (a, s)
getAll (State' sa) s = sa s

evalState :: State' s a -> s -> a
evalState (State' sa) = fst . sa

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

-- 预处理: 去除空行，空格， 缩进
preProcess :: String -> String
preProcess s = filter (\c -> not (c == ' ' || c == '\n' || c == '\t')) s

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
parseQuote :: Parser' String JValue
parseQuote = State' $ \s -> ((JString . before '\"') s, behind '\"' s)

parseArray :: Parser' String JValue
parseArray = State' $ \(c : cs) -> parseArray' [] (c : cs)
  where
    parseArray' jvs (x : xs) = case x of
        '\"' -> let (q, s) = (runState parseQuote) xs in parseArray' (q : jvs) s
        '{' -> let (q, s) = (runState parseObject) xs in parseArray' (q : jvs) s
        '[' -> let (q, s) = (runState parseArray) xs in parseArray' (q : jvs) s
        ']' -> (JArray jvs, xs)
        ',' -> parseArray' jvs xs
        _ -> let (q, s) = (runState parseLiteral) xs in parseArray' (q : jvs) s

-- parseObject :: Parser String JValue
parseObject :: Parser' String JValue
parseObject = State' $ \(c : cs) -> parseObject' [] (c : cs)
  where
    parseObject' jos (x : xs) = case x of
        '\"' ->
            let (q, s) = (runState parseQuote) xs
             in case s !! 1 of
                    '\"' -> let (q', s') = (runState parseQuote) (drop 2 s) in parseObject' ((q, q') : jos) s'
                    '[' -> let (q', s') = (runState parseArray) (drop 2 s) in parseObject' ((q, q') : jos) s'
                    '{' -> let (q', s') = (runState parseObject) (drop 2 s) in parseObject' ((q, q') : jos) s'
                    _ -> let (q', s') = (runState parseLiteral) (drop 1 s) in parseObject' ((q, q') : jos) s'
        ',' -> parseObject' jos xs
        '}' ->
            let jos' = map (\(a, b) -> (getString a, b)) jos
                getString (JString s) = s
             in (JObject jos', xs)

parseNonLiteral :: Parser' String JValue
parseNonLiteral = State' $ \(c : cs) -> case c of
    '\"' -> (runState parseQuote) cs
    '[' -> (runState parseArray) cs
    '{' -> (runState parseObject) cs
    _ -> (runState parseLiteral) cs

parseNumber :: Parser' String JValue
parseNumber = State' $ \(c : cs) -> parseNumber' [c] (cs)
  where
    readNumber cs = if isInfixOf "." cs then JDouble ((read cs) :: Float) else (JInt . read) cs
    parseNumber' ns (x : xs) = case x of
        ',' -> (readNumber ns, xs)
        _ -> parseNumber' (ns ++ [x]) xs

parseNull :: Parser' String JValue
parseNull = State' $ \(c : cs) -> parseNull' [c] cs
  where
    parseNull' ns (x : xs) = case x of
        'u' -> parseNull' (ns ++ [x]) xs
        'l' -> parseNull' (ns ++ [x]) xs
        ',' -> (readNull ns, xs)
          where
            readNull _ = JNull

-- false
-- true

parseBool :: Parser' String JValue
parseBool = State' $ \(c : cs) -> parseBool' [c] cs
  where
    parseBool' ns (x : xs) = if inBool x then parseBool' (ns ++ [x]) xs else ((readBool ns), xs)
      where
        inBool c = elem c "falsetrue"
        readBool cs =
            if cs == "false"
                then
                    JBool False
                else JBool True

parseLiteral :: Parser' String JValue
parseLiteral = State' $ \(c : cs) ->
    if isDigit c
        then (runState parseNumber) (c : cs)
        else
            if c == 'n'
                then (runState parseNull) (c : cs)
                else
                    if c == 't' || c == 'f'
                        then (runState parseBool) (c : cs)
                        else
                            (runState parseNonLiteral) (c : cs)

-- 布尔值 ： false true
-- 空值 ： null

--
-- parseLiteral :: Parser String JValue
-- parseLiteral (c : cs) = parseLiteral' [] (c : cs)
--   where
--     parseLiteral' jls (x : xs)
--         | isDigit x = parseNumber
--
--
-- #TODO
-- 完成数字解析: 浮点数 整 0-0
-- 空值 null
-- 布尔值 false true
-- 写出输入输出接口
-- 尝试设计查询字段解析：
--      查询字段设计 -》 解析 -》 调用
-- 失败处理？
--
-- arrayTest :: String
-- arrayTest = "\"abc\",[\"abc\",\"abc\"],[]]"
-- objectTest :: String
-- objectTest = "{\"a\":\"b\",\"a\":[\"c\"],\"a\":{\"c\":[\"a\"]}}"
objectTestNew = "{\"a\":\"b\",\"a\":true,\"a\":[\"c\"],\"a\":{\"c\":[\"a\"]}}"

parse :: String -> JValue
parse = fst . (runState parseObject) . (drop 1)
main :: IO ()
main = do
    p <- getLine
    c <- readFile p
    let a = preProcess c
    let o = parse a
    print o
