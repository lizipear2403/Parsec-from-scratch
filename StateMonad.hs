module StateMonad (
    StateT' (..),
    State' (..),
    Identity (..),
    Alternative (..),
    Parser' (..),
    ParserT (..),
    (<&>),
    evalState,
)
where

import Data.List (isInfixOf)

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

evalState :: State' s a -> s -> a
evalState (State' sa) = fst . sa

type ParserT s m a = StateT' s m a

(<&>) :: (Alternative m) => StateT' s m a -> StateT' s m a -> StateT' s m a
(StateT' sma1) <&> (StateT' sma2) = StateT' (sma1 <\> sma2)

class (Applicative f) => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    (<\>) :: (b -> f a) -> (b -> f a) -> b -> f a
    (<\>) f1 f2 b = liftA2 (<|>) f1 f2 b

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
