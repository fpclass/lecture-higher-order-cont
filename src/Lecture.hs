--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Higher-order functions (cont.)                                    --
--------------------------------------------------------------------------------

module Lecture where

import Prelude hiding ( and, product, length, foldl, (.) )

--------------------------------------------------------------------------------
-- folds (continued)

and :: [Bool] -> Bool
and []     = True
and (b:bs) = b && and bs

product :: Num a => [a] -> a
product []     = 1
product (n:ns) = n * product ns

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     = z
foldl f z (x:xs) = f (foldl f z xs) x

and'' :: [Bool] -> Bool
and'' = foldl (&&) True

product'' :: Num a => [a] -> a
product'' = foldl (*) 1

length'' :: [a] -> Int
length'' = foldl (\n x -> n + 1) 0

--------------------------------------------------------------------------------
-- count

-- count '3' "cs133" => 2
-- count :: (Num r, Eq a) => a -> [a] -> r
-- count _ [] = 0
-- count y (x:xs)
--     | y==x      = 1 + count y xs
--     | otherwise =     count y xs

-- count :: (Num r, Eq a) => a -> [a] -> r
-- count y = foldr (\x r -> if y==x then 1+r else r) 0

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

count y = length . filter (==y)

--------------------------------------------------------------------------------
