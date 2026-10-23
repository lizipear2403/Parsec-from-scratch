module Helper (
    isDigit,
    before,
    behind,
    isInfixOf,
) where

-- 判断是否包含中缀
isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf _ [] = True
isInfixOf [] _ = False
isInfixOf (h1 : ls1) (h2 : ls2) = if h1 == h2 then isInfixOf ls1 ls2 else False

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
