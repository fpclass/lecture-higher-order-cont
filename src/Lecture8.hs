--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 8: Fun with functions                                              --
--------------------------------------------------------------------------------

module Lecture8 where

import Prelude hiding ( and, product, length, foldr, foldl, (.) )

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

--------------------------------------------------------------------------------
