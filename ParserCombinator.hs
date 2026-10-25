module ParserCombinator (
    ParserT (..),
    Parser (..),
    between,
    consume,
    sepBy,
    genParser,
    pair,
    before,
) where

import Debug.Trace
import StateMonad

type ParserT s m a = StateT s m a
type Parser a b = State a b

doNothing :: ParserT String Maybe ()
doNothing = StateT $ \s -> Just ((), s)

-- 解析单个字符并且不返回值
consume :: Char -> ParserT String Maybe ()
consume s = StateT $
    \(c : str) -> if c == s then Just ((), str) else Nothing

-- 将某个符号之前的所有字符全部解析
before :: Char -> ParserT String Maybe a -> ParserT String Maybe a
before c p = StateT $ \(ss) ->
    let (a, b) = break (== c) ss
     in do
            (v, s) <- runStateT p a
            return (v, b)

-- 根据传入的解析器集合生成优先级解析子
genParser :: (Alternative m, Monad m) => [ParserT s m a] -> ParserT s m a -> ParserT s m a
genParser ps dp = foldl' (<&>) dp ps

-- 根据某个字符的解析进行分割
sepBy :: ParserT String Maybe () -> ParserT String Maybe a -> ParserT String Maybe [a]
sepBy sep vp = StateT $ \str -> sep' str (Just [])
  where
    sep' s (Just jvs) = do
        (v, s1) <- runStateT vp (s)
        case runStateT sep (s1) of
            Nothing -> Just ((v : jvs), s1)
            (Just (_, s')) -> sep' s' (Just (v : jvs))

-- 期待一个分隔符？
-- 没有分隔符 donothing -> 原样传出
-- >>= \(_, s') -> sep' s' (Just (v : jvs))

-- 根据某个字符来成对解析
pair ::
    Char ->
    (ParserT String Maybe a, ParserT String Maybe a) ->
    ParserT
        String
        Maybe
        (a, a)
pair sp (pfst, psnd) = StateT $ \(str) -> do
    (k, (s : s')) <- runStateT pfst str
    if s == sp
        then do
            (v, s'') <- runStateT psnd s'
            return $ ((k, v), s'')
        else
            Nothing

-- 在某两个符号之间进行解析
between ::
    ParserT String Maybe () ->
    ParserT String Maybe a ->
    ParserT String Maybe () ->
    ParserT String Maybe a
between ps pa pe = StateT $ \str -> do
    (_, s) <- runStateT ps str
    (a, s') <- runStateT pa s
    (_, s'') <- runStateT pe s'
    return (a, s'')
