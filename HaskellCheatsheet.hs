hs notes

lists - head tail last init
[1,2,3] !! 1  --2
1:2:3:[4,5,6] --[1,2,3,4,5,6]
length [1,2,3] --3
null [] --True
reverse [1,2,3] --[3,2,1]
take 3 [1,2,3,4,5] --[1,2,3]
3 `take` [1,2,3,4] --[1,2,3]
drop 1 [1,2,3] --[2,3]
maximum [1,2,3] --3
minimum [1,2,3] --1
sum [1,2,3] --6
product [1,2,3,4] --24
elem 3 [1,2,3] --True
[1..5] --[1,2,3,4,5]
['a'..'c'] --['a','b','c']
[1,3..9] --[1,3,5,7,9]
take 5 (cycle [1,2,3]) --[1,2,3,1,2]
take 3 (repeat 0) --[0,0,0]
replicate 3 10 --[10,10,10]
[x*2 | x <- [1,2,..]] --[2,4,6,..]
take 2 [x*2 | x <- [1,2,..]] --[2,4]
take 2 [x*2 | x <- [1,2,..], mod x 2 == 1] --[2,6]
take 4 [x | x <- [1,2..], x /= 3, x /= 4, x /= 5] --[1,2,6,7]
take 5 [x+y | x <- [1,2..], y <- [2,4,..]] --[3,6,9,12,15]
length' xs = sum [1 | _ <- xs]
(1,2)
[(1,2),(2,3),(4,7)] --an array of tuples, b/c [[1,2], [2,3,4], [4,7]] is possible, and leaves a hole for logic errors
:t (1,2) --(1,2) :: (Num t, Num t1) => (t, t1)
[(1,2),("One",2)] -- ERROR,not possible. Lists are homogenous.
fst (1,2,3) --1
snd (1,2,3) --2
zip [1..5] "abcde" --[(1,'a'),(2,'b'),..]
zip [1..] "abc" --[(1,'a'), (2,'b'), (3,'c')] terminates at end
let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] --computes right triangles***
--If a function is comprised only of special characters, it's considered an infix function by default; but it can be used as a prefix operator still
3 == 3
(== 3 3)
--Typeclasses
(Eq a) => a -> a -> Bool -- ==
(Ord a) => a -> a -> Bool -- <, >, >=, <=
compare 1 2 -- LT, EQ, or GT
1 `compare` 2
(Show a) => a -> String -- Show typeclass can be converted to String
show True --"True"
:t read --Read a => String -> a
read "[1,2,3]" ++ [4] --[1,2,3,4]
read "[1,2,3]" -- Error: GHC doesn't know what type operation to run on the String
-- explicit type annotations
read "5" :: Int
read "5" :: Float
read "[1,2,3,4]" :: [Int] --[1,2,3,4]
read "(3, 'a')" :: (Float, Char) --(3.0,'a')
-- Enum types - can be used in ranges [a..z]
-- includes (), Bool, Char, Ordering, Int, Integer, Float and Double
succ 'A' -- 'B'
pred 'B' -- 'A'
['a'..'z']
[LT..GT]
-- Bounded typeclass
:t minBound -- Bounded a => a
minBound :: Int -- -2147483648
maxBound :: Char -- '\1114111'
maxBound :: (Bool, Int, Char) -- (True,2147483647,'\1114111')
-- Num typeclass
:t 20 -- 20 :: (Num t) => t
20 :: Integer -- 20
20 :: Double -- 20.0
:t * -- (*) :: (Num a) => a -> a -> a
-- Integral - Ints and Integers
-- Floating - Floats and Doubles
:t fromIntegral -- fromIntegral :: (Num b, Integral a) => a -> b
-- useful for doing things like
length [1,2,3] + 1 -- Error, trying to add Int and Num
:t length -- length :: [a] -> Int
:t + -- (+) :: (Num a) => a -> a -> a
:t fromIntegral -- fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral(length [1,2,3]) + 1 -- 4
-- pattern matching
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
-- pattern matching with list comprehensions
[a+b | (a,b) <- xs]
-- _ doesn't get bound
third :: (a, b, c) -> c
third (_, _, z) = z
-- fn overloading
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
-- (x:[]) and (x:y:[]) could be rewritten as [x] and [x,y].
-- We can't rewrite (x:y:_) with square brackets because it matches any list of length 2 or more.
--
-- how about a pattern-matched recursive length fn?
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
-- and sum?
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
-- as patterns @
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [capitalize x]
-- Guards
segment :: (Floating a) => a -> String
segment a
    | a < .1 = "Side 1"
    | a < .9 = "Side 2"
    | a <  1 = "Side 3"
    | otherwise = "Not a side"
-- multiple inputs
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b
-- one liner
max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b
-- where clause
max' :: (Ord a) => a -> a -> a
max' a b
    | n > 0 = a
    | otherwise = b
    where n = a-b
-- multi-where clause
max' :: (Ord a) => a -> a -> a
max' a b
    | n > 0 = x
    | otherwise = y
    where n = a-b
        x = a
        y = b
-- can even pattern-match!!!
max' :: (Ord a) => a -> a -> a
max' a b
    | n > 0 = x
    | otherwise = y
    where (n,x,y) = (a-b,a,b)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- in addition to where-bindings, there are let bindings
cylinder :: (Floating a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea
-- these also support pattern-matching
(let (a,b,c) = (1,2,3) in a+b+c) * 100 -- 600
-- and can be put into list comprehensions
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
-- case expressions (similar to method overloading)
head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x
-- can be written as
head' :: [a] -> a
head' xs = case xs of
    [] -> error "No head for empty lists!"
    (x:_) -> x
-- case expressions can be written anywhere
describeList :: [a] -> String
describeList xs =
    "The list is " ++
    case xs of
        [] -> "empty."
        [x] -> "a singleton list."
        xs -> "a longer list."
-- even in where clauses
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
--rewriting as a regular fn for practice
describeList :: [a] -> String
describeList [] = "The list is empty."
describeList [x] = "The list is a singleton list."
describeList xs = "A longer list."
-- and with guards
describeList :: [a] -> String
describeList xs
    | len == 0 = "empty"
    | len == 1 = "singleton"
    | otherwise = "longer list"
    where len = length xs
{-
    Recursion
-}
-- pattern matching goes great w/ recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
-- or.......
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
-- replicate
replicate' :: (Num i, Ord i) => i -> a -> [a] -- Num is not a subclass or Ord, so we have to specify both
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x
-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
-- quicksort!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted
{- Haskell is auto-curried -}
max 4 5 --5
(max 4) 5 --5
:t max -- max :: (Ord a) => a -> a -> a
-- same as max :: (Ord a) => a -> (a -> a)
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
-- could be written as
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100
-- ...since compareWithHundred is a function that takes another Ord
{- Sectioning an infix operator -}
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
multByTen = (*10)
multByTen 5 -- 50
isUpperAlphanum = (`elem` ['A'..'Z'])
isUpperAlphanum 'B' -- True
{-Haskell also supports higher order functions-}
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
applyTwice (+ 1) 1 -- 3
applyTwice (++ " HAHA") "HEY" --"HEY HAHA HAHA"
applyTwice ("HAHA " ++) "HEY" --"HAHA HAHA HEY"
-- zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- flip
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
flip` (++) "test" "best" --"besttest"
flip' zip [1,2,3,4,5] "hello" --[('h',1),('e',2),('l',3),('l',4),('o',5)]
zipWith (flip' div) [2,2..] [10,8,6,4,2] --[5,4,3,2,1]
-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
map (+3) [1,5,3,1,6] --[4,8,6,4,9]
map (replicate 3) [3..6] --[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
map fst [(1,2),(3,5),(6,3),(2,6),(2,5)] --[1,3,6,2,2]
-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = (if p x then [x] else []):(filter p xs)
-- a new quicksort!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted
-- we can count backwards, and find the largest number under 100k that's divisible by 3829
let p x = rem x 3829 == 0 in head (filter p [100000,99999..])
-- takeWhile
takeWhile (/=' ') "elephants know how to party" --"elephants"
takeWhile (<100) (filter odd (map (^2) [1..])) --[1,9,25,...]
takeWhile (<100) [n^2 | n <- [1..], odd (n^2)] --[1,9,25,...]
-- generating our own sequences - Collatz seq (if x is 1, return, else if x is odd, return 3x + 1, else x/2)
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (div n 2)
    | odd n  =  n:chain (n*3 + 1)
take 5 (chain 10) --[10,5,16,8,4]
chain 10 --[10,5,16,8,4,2,1]
-- for all n in [1..100], find the chains with more than 15 numbers
longChains :: (Int n) => n -> [[Int]]
longChains len = filter isLong (map chain [1..100])
    where isLong xs = length xs > len
numLongChains :: Int -> Int
numLongChains len = length (longChains len)
-- again on currying, we can return a bonary function that maps to one input
let listOfFuns = map (*) [0..] --[0*,1*,2*,3*,4*,...]
(listOfFuns !! 4) 5 --4*5
-- lambda functions
take 5 (map (\x -> x^2) [1..]) --[1,4,9,16,25]
-- using a lambda in Collatz sequence
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
-- lambdas take multiple inputs, and can pattern match
(\a b -> a + b) 4 2 --6
map (\x -> x + 3) [1,6,3,2] --[4,9,6,5]
zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5] --[153.0,61.5,31.0,15.75,6.6]
map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)] --[3,8,9,8,7]
-- lambdas extend all the way to the right unless wrapped in parens
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z
-- equiv to
addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z
-- ^^^ makes it obvious why type declarations look the way they do
-- flip, for example:
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x
-- foldl -- like reduce in JS
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' [3,5,2,1] -- 11
-- a better version
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0
-- in otherwords (\acc x -> acc + x) is an alias for (+)
-- we can even implement the elem fn
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys
-- foldr flips the input order to the callback fn,
-- foldl passes \acc val, foldr passes \val acc to the callback
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- The foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
-- scanl, scanl1, scanr, scanr1 do the fold, but they also return an array of all previously accumulated values
scanl (+) 0 [3,5,2,1] --[0,3,8,10,11]
scanr (+) 0 [3,5,2,1] --[11,8,3,1,0]
scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1] --[3,4,5,5,7,9,9,9]
scanl (flip (:)) [] [3,2,1] --[[],[3],[2,3],[1,2,3]]
-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
sqrtSums --131

-- $ - function application, left associative so that it has the lowest precedence
($) :: (a -> b) -> a -> b
f $ x = f x
f a b c
-- is the same as..
(((f a) b) c)
-- but
f $ a b c
-- is the same as
f ((a b) c)

sum (map sqrt [1..130]) -- is the same as
sum $ map sqrt [1..130]

sqrt (3 + 4 + 9) -- is the same as
sqrt $ 3 + 4 + 9

sum (filter (> 10) (map (*2) [2..10])) -- is the same as
sum $ filter (> 10) $ map (*2) [2..10]

-- $ also means fn application can be treated just like another fn
map ($ 3) [(4+), (10*), (^2), sqrt] -- [7.0, 30.0, 9.0, 1.7320...]

-- . mean function composition
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

map (negate . abs) [5,-3,-6,7,-3,2,-19,24] --[-5,-3,-6,-7,-3,-2,-19,-24]

f (g (z x)) -- is equiv to
(f . g . z) x

-- so
map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]] --[-14,-15,-27]
-- is equiv to
map (negate . sum . tail) [[1..5],[3..6],[1..7]]

-- using the fn composition (.) operator induces what we call "point-free style"
-- we solved a problem of finding the sum of all odd squares that are smaller than 10,000. Here's what the solution looks like when put into a function
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- this is way better!!!!!
oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- MODULES
import Data.List -- nub gets imported into the global namespace
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub -- equiv to \xs -> length (nub xs)

-- in GHCI, you can put a module's fns into the global namespace with...
:m + Data.List
:m + Data.List Data.Map Data.Set

import Data.List (nub, sort) -- import only nub, sort
import Data.List hiding (nub) -- import all but nub (i.e. if we already have a nub fn in our module)
-- if we've already imported Data.List and it's filter fn, and then....
import qualified Data.Map
-- we can reference Data.Map's filter with...
Data.Map.filter

-- can also do
import qualified Data.Map as M
M.filter

-- Data.List Module
intersperse '.' "MONKEY" -- "M.O.N.K.E.Y"
intersperse 0 [1,2,3,4,5,6] -- [1,0,2,0,3,0,4,0,5,0,6]
intercalate " " ["hey","there","guys"] -- "hey there guys"
intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]] -- [ 1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]
transpose [[1,2,3],[4,5,6],[7,8,9]] -- [[1,4,7],[2,5,8],[3,6,9]]
transpose ["hey","there","guys"] -- ["htg","ehu","yey","rs","e"]
-- transpose is really cool, imagine we're adding 3x^2 + 5x + 9, 10x^3 + 9, and 8x^3 + 5x^2 + x -1
map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]] -- [18,8,6,17]
-- foldl' and foldl1' are strict versions of foldl and foldl
foldl1' (\a x-> a + x) [1,2,3] --[6] (eager computation)
foldl1 (\a x-> a + x) [1,2,3] --[6] (lazy computation (intermediate values are thunks))
concat ["foo", "bar", "car"] --"foobarcar"
concat [[1,2,3],[4,5,6],[7,8,9]] --[1,2,3,4,5,6,7,8,9]
concatMap (replicate 2) [1..3] --[1,1,2,2,3,3,4,4]
and $ map (>4) [5,6,7,8] --True
and $ map (==4) [4,4,3] --False
or $ map (==4) [1,2,3,4] --True
any (==4) [1,2,3,4] --True (4 iterations)
any (==4) [4,3,2,1] --True (1 iteration)
all (>4) [6,9,10] --True
all (`elem` ['A'..'Z']) "HEYGUYSwhatsup" --False
any (`elem` ['A'..'Z']) "HEYGUYSwhatsup" --True
take 10 $ iterate (*2) 1 --[1,2,4,8,16,32,64,128,256,512]
take 3 $ iterate (++ "hee") "haha" --["haha", "hahahee", "hahaheehee"]
splitAt 3 "heyman" --("hey","man")
splitAt 100 "heyman" --("heyman","")
splitAt (-3) "heyman" --("", "heyman")
let (a,b) = splitAt 3 "foobar" -- ("foo", "bar")
let (a,b) = splitAt 3 "foobar" in b ++ a -- "barfoo"
takeWhile (>3) [6,5,4,3,2,1] --[6,5,4]
takeWhile (/=' ') "This is a sentence" --"This"
-- find the sum of all third powers under 10000
sum $ takeWhile (<10000) $ map (^3) [1..]
dropWhile (/=' ') "This is a sentence" --"is a sentence"
dropWhile (< 4) [1,2,3,4,3,2,1] -- [4,3,2,1]
-- when did the stock value first reach $1000?
let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
head $ dropWhile (\(val,y,m,d) -> val < 1000) stock
-- span is like takeWhile, but returns a pair of lists - (beforeDropPoint, afterDropPoint)
span (/=' ') "This is a sentence" --("This" ,"is a sentence")
-- span does a takeWhile, break does a takeWhileNot
break (==4) [1,2,3,4,5,6,7] --([1,2,3], [4,5,6,7])
-- same as...
span (/=4) [1,2,3,4,5,6,7] --([1,2,3], [4,5,6,7])
sort [8,5,3,2,1,6,4,2] --[1,2,3,4,5,6,7,8] sorts Ord types
sort :: (Ord a) => [a] -> [a]
-- group collects adjacent values that are equal
group [1,1,1,2,2,2,1,2,2,2] --[[1,1,1,1], [2,2,2], [1], [2,2,2]]
-- how many times does an element show up in a list?
map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] -- [(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
init "woot" -- "woo"
inits "woot" -- ["", "w", "wo", "woo", "woot"]
tail "woot" -- "oot"
tails "woot" -- ["woot", "oot", "ot", "t", ""]
let w = "woot" in zip (inits w) (tails w) --[("", "woot"), ("w", "oot"), ("wo", "ot"), ...]
isInfixOf "cat" "meow cat meow" --True
"cat" `isInfixOf` "meow cat meow" --True
"Cat" `isInfixOf` "meow cat meow" --False
isPrefixOf "hey" "hey there" --True
isSuffixOf "there!" "hey there!" --True
elem 'a' ['a'..'z'] --True
elem 'a' "cat" --True
notElem 'a' ['b'..'z'] --True
partititon (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" --("BOBMORGAN", "sidneyeddy")
partition (>3) [1,3,5,6,3,2,1,0,3,7] --([5,6,7], [1,3,3,2,1,0,3])
-- differs from span...
span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" --("BOB", "sidneyMORGANeddy")
-- find takes a list, a pred, and returns the first element that satisfies the pred.  A Maybe can be either `Just something` or `Nothing`
find (>4) [1,2,3,4,5,6] -- Just 5
find (>9) [1,2,3,4,5,6] -- Nothing
:t find --find :: (a -> Bool) -> [a] -> Maybe a
head (dropWhile (\(val,y,m,d) -> val < 1000) stock) -- what if stocks were never less than 1000? then head [] would cause an error, we use Maybe for this
find (\(val,y,m,d) -> val > 1000) stock -- is better, returns a Maybe, could be Just (1001.4,2008,9,4) or Nothing
:t elemIndex --(Eq a) => a -> [a] -> Maybe Int
4 `elemIndex` [1,2,3,4] -- Just 3
10 `elemIndex` [1,2,3,4] -- Nothing
' ' `elemIndices` "Where are the spaces?" --[5,9,13]
' ' `elemIndices` "cat" --[] elemIndices can just return an empty list
findIndex (==4) [5,4,3,2,1,4] -- Just 1
findIndex (==40) [5,4,3,2,1,4] -- Nothing
findIndices (==4) [5,4,3,2,1,4] --[1,5]
--zip and zipWith zip up two lists into tuples; there's also zip3, zip4, zipWith3, zipWith4
zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3] --[7,9,8]
zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2] --[(2,2,5,2), (3,2,5,2), (3,2,3,2)]
lines "first\nsecond\nthird" --["first", "second", "third"]
unlines ["first", "second", "third"] --"first\nsecond\nthird"
words "hello there" --["hello", "there"]
words "hello         there, \nmate" --["hello", "there", "mate"]
unwords ["hello", "there"] --"hello there"
-- remember that nub weeds out duplicates
nub [1,2,2,3,4,3] -- [1,2,3,4]
nub "Lots of words" --"Lots fwrd"
delete 'h' "hey there ghang!" --"ey there ghang!"
delete 'h' . delete 'h' "hey there ghang!" --"ey tere ghang!"
delete 'h' . delete 'h' . delete 'h' "hey there ghang!" --"ey tere gang!"
\\ -- the difference function
[1..10] \\ [2,5,9] --same as doing delete 2 . delete 5 . delete 9 $ [1..10]
union "hey man" "man what's up" -- "hey manwt'sup"
union [1..7] [5..10] --[1,2,3,4,5,6,7,8,9,10]
intersect [1..7] [5..10] --[5,6,7]
insert 4 [3,5,1,2,8,2] --[3,4,5,1,2,8,2] -- inserts val into first position where it's <= the next element, and > prev element
insert 4 [1,3,4,4,1] --[1,3,4,4,4,1]
insert 'g' $ ['a'..'f'] ++ ['h'..'z'] --"abcdefghijklmonpqrstuvwxyz"
insert 3 [1,2,4,3,2,1] --[1,2,3,4,3,2,1]
--Data.List has generic equivalents to length, take, drop, splitAt, !!, and replicate, which take an Int and return an Int. Instead, they could take typeclasses (Integral, Num) with Data.List's genericLength, genericTake, genericDrop, genericSplit, genericIndex, genericReplicate
:t length --[a] -> Int
--can't use Ints with division, i.e. can't do
let xs = [1..6] in sum xs / length xs
--but can do
let xs = [1..6] in sum xs / genericLength xs
--because gneericLength ...
:t genericLength --(Num a) => [b] -> a
--nub, delete, union, intersect, group also have their own generics:
nubBy
deleteBy
unionBy
intersectBy
groupBy
-- the built-ins use (==) for comparison, but these all take a comparision fn... i.e. the built in group is actually just:
groupBy (==)
-- so we can even group values by say, positive and negative values
let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
groupBy (\x y -> (x > 0) == (y > 0)) values --[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
-- one cleaner way of using these by functions, groupBy, etc... is to use the on function from Data.Function
:m + Data.Function
:t on -- (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g = \x y -> f (g x) (g y)
--so ...
(==) `on` (> 0) -- returns an equality function that looks like
\x y -> (x > 0) == (y > 0)
-- so we can do
groupBy ((==) `on` (> 0)) values -- [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
-- you might call f the map, and g the compare fn's
let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
sortBy (compare `on` length) xs --[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
--Data.Char module
:m + Data.Char
-- or
import Data.Char
--all of the following or Char -> Bool
isControl
isSpace
isLower
isUpper
isAlpha
isAlphaNum
isPrint
isDigit
isOctDigit
isHexDigit
isLetter
isMark
isNumber
isPunctuation
isSymbol
isSeparator
isAscii
isLatin1
isAsciiUpper
isAsciiLower
all isAlphaNum "bobby283" --True
all isAlphaNum "eddy the fish!" --False
words "hey guys its me" --["hey","guys","its","me"]
groupBy ((==) `on` isSpace) "hey guys its me" --["hey"," ","guys"," ","its"," ","me"]
filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me" --["hey","guys","its","me"]
--GeneralCategory type, generalCategory :: Char -> GeneralCategory
generalCategory ' ' --Space
generalCategory 'A' --UppercaseLetter
generalCategory 'a' --LowercaseLetter
generalCategory '.' --OtherPunctuation
generalCategory '9' --DecimalNumber
map generalCategory " \t\nA9?|" --[Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]
map digitToInt "34538" --[3,4,5,3,8]
map digitToInt "FF85AB" --[15,15,8,5,10,11]
intToDigit 15 --'f'
intToDigit 5 --'5'
ord 'a' --97
chr 97 --'a'
map ord "abcdefgh" --[97,98,99,100,101,102,103,104]
encode :: Int -> String -> String
encode shift msg = map (chr . (+ shift) . ord) msg
encode 3 "Heeeeey" --"Khhhhh|"
encode 4 "Heeeeey" --"Liiiii}"
encode 1 "abcd" --"bcde"
encode 5 "Marry Christmas! Ho ho ho!" --"Rfww~%Hmwnxyrfx&%Mt%mt%mt&"
decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
encode 3 "Im a little teapot" --"Lp#d#olwwoh#whdsrw"
decode 3 "Lp#d#olwwoh#whdsrw" --"Im a little teapot"
decode 5 . encode 5 $ "This is a sentence" --"This is a sentence"
--Data.Map
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
--throws a runtime error if key isn't in the list
--so let's try a new one
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs
-- can also be implemented as a fold
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
--Because Data.Map exports functions that clash with the Prelude and Data.List ones, we'll do a qualified import.
import qualified Data.Map as Map
--or
:m + Data.Map
Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] --fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
Map.fromList [(1,2),(3,4),(3,2),(5,5)] --fromList [(1,2),(3,2),(5,5)]

:t Map.fromList --Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v
--empty represents an empty list
Map.empty --fromList []
--insert
Map.insert 3 100 Map.empty --fromList [(3,100)]
Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty)) --fromList [(3,100),(4,200),(5,600)]
Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty --fromList [(3,100),(4,200),(5,600)]
--fromList...
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
Map.null Map.empty --True
Map.null $ Map.fromList [(2,3),(5,5)] --False
Map.size Map.empty --0
Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)] --5
Map.singleton 3 9 --fromList [(3,9)]
Map.insert 5 9 $ Map.singleton 3 9 --fromList [(3,9),(5,9)]
--Map.lookup works like List.lookup
Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)] --True
Map.member 3 $ Map.fromList [(2,5),(4,5)] --False
Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)] --fromList [(1,100),(2,400),(3,900)]
Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')] --fromList [(2,'A'),(4,'B')]
Map.toList . Map.insert 9 2 $ Map.singleton 4 3 --[(4,3),(9,2)]
--Map.keys, Map.elems return lists of keys and values respectively.
--Map.keys is equiv to
map fst . Map.toList
--and Map.elems is equiv to
map snd . Map.toList
--fromListWith can be a map, but handles multiple values mapped to same kay
phoneBook =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

Map.lookup "patsy" $ phoneBookToMap phoneBook --"827-9162, 943-2929, 493-2928"
Map.lookup "wendy" $ phoneBookToMap phoneBook --"939-8282"
Map.lookup "betty" $ phoneBookToMap phoneBook --"342-2492, 555-2938"

--could also be
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

Map.lookup "patsy" $ phoneBookToMap phoneBook --["827-9162","943-2929","493-2928"]

--duplicate key found, but we want the biggest value
Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)] --fromList [(2,100),(3,29),(4,22)]
Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)] --fromList [(3,104),(5,103),(6,339)]

--Data.Set (mirrors a lot of Map above, and just like Map, Sets are implemented with Trees as data-structures; only values don't have keys, and they exist uniquely in the Set)
import qualified Data.Set as Set
Set.fromList "hello there" -- fromList "helotr"
Set.intersection (Set.fromList "hi") (Set.fromList "hello") --fromList "h"
Set.difference
Set.union
Set.null
Set.empty
Set.size
Set.singleton
Set.insert
Set.delete
Set.isSubsetOf
Set.isProperSubsetOf
Set.filter
Set.map
Set.toList

--creating our own modules
--create a module header Geometry that specifies the functions in the file to be exported
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where
--...

--import a module
module Geometry.Cube
( volume
, area
) where

import Geometry.Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side

--import a module, namespaced
module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side

-- Defining our own Typeclasses
-- use the keyword `data` to do so
data Matt = False | True
-- create shape constructors... Circle takes 3 inputs, Rectangle takes 4 inputs
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
:t Circle --Circle :: Float -> Float -> Float -> Shape
:t Rectangle --Rectangle :: Float -> Float -> Float -> Float -> Shape
-- let's make a fn that takes a Shape and returns its surface
surface :: Shape -> Float --takes a Shape, returns a Float
surface (Circle _ _ r) = pi * r ^ 2 --if input is a Circle... we can pattern-match against constructors!!
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) --if input is a Rectangle...

surface $ Circle 10 20 10 --314.15927
surface $ Rectangle 0 0 100 100 --10000.0

Circle 10 20 5 -- Error!!! Haskell doesn't know how to display our type in CLI, to support `show Circle...` we need to make our Shape type part of the Show typeclass:

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

--now we can do this...
Circle 10 20 5 --Circle 10.0 20.0 5.0
Rectangle 50 230 60 90 --Rectangle 50.0 230.0 60.0 90.0

--value constructors are functions, so they can be curried / partially applied
map (Circle 10 20) [4,5,6,6] --[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

--let's make our Shape use a Point type instead of Floats, which will hold an X and Y value
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

--let's update surface to use these types
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

--nudge a shape
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

--can export all these like so...
module Shapes
( Point(..) --
, Shape(..) -- exporting the (..) exports the constructor fn's for a Circle and Rectangle, same as doing Shape(Point,Rectangle)
--just writing Shape without subtypes will not export the constructors, instead relying on a fromFloat or other function, just like Map does. There is now Map.Map fn, but there is a Map.fromList
, surface
, nudge
) where

--Records
data Person = Person String String Int Float String String deriving (Show) --describes a Person type
let guy = Person "Matt" "Keas" 28 180.0 "832.312.2288" "rocky road"
-- WHAT DO THESE VALUES STAND FOR?
-- getting the firstname out of this type is a bit confusing
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname
firstname guy --"Matt"
--instead we can use records to make this easier
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

:t flavor --flavor :: Person -> String
:t firstName --firstName :: Person -> String

--if we created guy with this...
let guy = Person {firstName="Matt", age=28, ...}

--Type Parameters
data Maybe a = Nothing | Just a
-- because a is a paremeter to a function, we call Maybe a "type constructor"

--playing around with the Maybe type
Just "Haha" --Just "Haha"
Just 84 --Just 84
:t Just "Haha" --Just "Haha" :: Maybe [Char]
:t Just 84 --Just 84 :: (Num t) => Maybe t
:t Nothing --Nothing :: Maybe a
Just 10 :: Maybe Double --Just 10.0

--accessing properties of a record

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

let stang = Car {company="Ford", model="Mustang", year=1967}
tellCar stang --"This Ford Mustang was made in 1967"

-- Vector implementation
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

Vector 3 5 8 `vplus` Vector 9 2 8 --Vector 12 7 16
Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3 --Vector 12 9 19
Vector 3 9 7 `vectMult` 10 --Vector 30 90 70
Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0 --74.0
Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4) --Vector 148 666 222

--But right now, let's see how Haskell can automatically make our type an instance of any of the following typeclasses: Eq, Ord, Enum, Bounded, Show, Read.
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving(Eq)
-- this describes a Person, and it makes sense to derive Eq because we might want to compare someone's name and age
-- to compare two Persons with == and /=, ALL FIELDS MUST HAVE TPES THAT ARE ALSO EQUATABLE
let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
mca == adRock --False

let beastieBoys = [mca, adRock]
adRock `elem` beastieBoys --True

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read) -- deriving Show and Read makes it possible to

let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
mikeD --Person {firstName = "Michael", lastName = "Diamond", age = 43}
"mikeD is: " ++ show mikeD --"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"

read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person --Person {firstName = "Michael", lastName = "Diamond", age = 43}
read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD --True

--Types and (Ord)ering
data Bool = False | True deriving (Ord) --because True comes after False, True is considered greater than False
True `compare` False --GT
True > False --True
True < False --False

--In the Maybe a data type, the Nothing value constructor is specified before the Just value constructor, so a value of Nothing is always smaller than a value of Just something, even if that something is minus one billion trillion
Nothing < Just 100 --True
Nothing > Just (-49999) --False
Just 3 `compare` Just 2 --GT
Just 100 > Just 50 --True
--But we can't do something like Just (*3) > Just (*2), because (*3) and (*2) are functions, which aren't instances of Ord.

--Enum and Bounded typeclasses
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)
--becuase of Show and Read
Wednesday --Wednesday
show Wednesday --"Wednesday"
read "Saturday" :: Day --Saturday
--because we derive Ord and Eq, we can compare
Saturday == Sunday --False
Saturday == Saturday --True
Saturday > Friday --True
Monday `compare` Wednesday --LT
-- because it's Bounded
minBound :: Day --Monday
maxBound :: Day --Sunday
-- because it's an Enum
succ Monday --Tuesday
pred Saturday --Friday
[Thursday .. Sunday] --[Thursday,Friday,Saturday,Sunday]
[minBound .. maxBound] :: [Day] --[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

-- Type Synonyms
[Char]
--equiv to
String

-- these are type synonyms
type String = [Char]

--we can rewrite our phonebook like so
phoneBook :: [(String,String)]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]
--can become
type PhoneBook = [(String,String)]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

--now we can give a pretty eloquent type descript
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

-- let's create an assoc list synonym for an array of typles
type AssocList k v = [(k,v)]
--Fonzie says: Aaay! When I talk about concrete types I mean like fully applied types like Map Int String or if we're dealin' with one of them polymorphic functions, [a] or (Ord a) => Maybe a and stuff. And like, sometimes me and the boys say that Maybe is a type, but we don't mean that, cause every idiot knows Maybe is a type constructor. When I apply an extra type to Maybe, like Maybe String, then I have a concrete type. You know, values can only have types that are concrete types! So in conclusion, live fast, love hard and don't let anybody else use your comb!


type IntMap = Map Int
data H = IntMap String

makeH :: (H h) => Int -> String -> h
let makeH a b = Map.fromList [(1,"a")]

--Another cool data type that takes two types as its parameters is the Either a b type. This is roughly how it's defined:
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

Right 20 --Right 20
Left "w00t" --Left "w00t"
:t Right 'a' --Right 'a' :: Either a Char
:t Left True --Left True :: Either Bool b

import qualified Data.Map as Map
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

--lookup lockers
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
-- Left is used to hold an error, Right is used to return the code

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]
lockerLookup 101 lockers --Right "JAH3I"

--Recursive data structures and lists
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
--record syntax...
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
--cons is another word for :

Empty --Empty
5 `Cons` Empty --Cons 5 Empty
4 `Cons` (5 `Cons` Empty) --Cons 4 (Cons 5 Empty)
3 `Cons` (4 `Cons` (5 `Cons` Empty)) --Cons 3 (Cons 4 (Cons 5 Empty))

--automatically make fn's infix by making the name comprise of only special characters
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
--A fixity states how tightly the operator binds and whether it's left-associative or right-associative, and the order of precedence (higher the more precedent)
--For instance, *'s fixity is infixl 7 * and +'s fixity is infixl 6. That means that they're both left-associative (4 * 3 * 2 is (4 * 3) * 2) but * binds tighter than +, because it has a greater fixity, so 5 * 4 + 3 is (5 * 4) + 3
--we just wrote a :-: (List a) instead of Cons a (List a
3 :-: 4 :-: 5 :-: Empty  --(:-:) 3 ((:-:) 4 ((:-:) 5 Empty))
let a = 3 :-: 4 :-: 5 :-: Empty
100 :-: a --(:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))

--++ is defined like so
infixr 5 ++
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

--stealing that definition for our custom list above with :-:
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

let a = 3 :-: 4 :-: 5 :-: Empty
let b = 6 :-: 7 :-: Empty
a .++ b --(:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty))))

--Notice how we pattern matched on (x :-: xs). That works because pattern matching is actually about matching constructors.

--implementing Binary Search Trees
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
--a tree is either an empty tree or it's an element that contains some value and two trees

--So, here are two functions. One is a utility function for making a singleton tree (a tree with just one node) and a function to insert an element into a tree
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
let nums = [8,6,4,1,7,3,5]
let numsTree = foldr treeInsert EmptyTree nums
numsTree --Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

8 `treeElem` numsTree --True

--The data keyword creates a typeclass consisting of sub-typeclasses... but what if we want to create our own subtypeclass? One that is atomic in nature?
--This is how Eq is defined (new syntax approaches!!)
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

--example, simulate a traffic light
data TrafficLight = Red | Yellow | Green
--nothing derived (like Eq, Ord, Show, Print,..., instead we can make it an instance of Eq like so
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
{-
recap:

- class defines new typeclasses
- data is for defining the values a type can have
- derive is used with data to attach typeclasses to it
- and instance is for making our types instances of a typeclass

above, in the instance, we only defined the equality (==), not inequality (/=), because the `class Eq a where` declaration also defines a relationship b/w

    x == y = not (x /= y)  and
    x /= y = not (x == y)

    thus, we tied the two of them together, and Haskell can infer values from it

Because == was defined in terms of /= and vice versa in the class declaration, we only had to overwrite one of them in the instance declaration. That's called the minimal complete definition for the typeclass — the minimum of functions that we have to implement so that our type can behave like the class advertises

-}

-- have TrafficLight implement the Show interface
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

Red == Red --True
Red == Yellow --False
Red `elem` [Red, Yellow, Green] --True
[Red, Yellow, Green] --[Red light,Yellow light,Green light]

--You can also make typeclasses that are subclasses of other typeclasses
--i.e. Num implements Eq like so
class (Eq a) => Num a where
    ...
--basically means we need to make a an instance of Eq before it can be an instance of Num
--When defining function bodies in the class declaration or when defining them in instance declarations, we can assume that a is a part of Eq and so we can use == on values of that type.

-- we can make Maybe's an instance of Eq, too
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

--m has to be Eq as well, since we compare x and y

--class constraints in _class declarations_ are used for making a typeclass a subclass of another typeclass
--and class constraints in _instance declarations_ are used to express requirements about the contents of some type

{-
Ooh, one more thing, check this out!

1. If you want to see what the instances of a typeclass are, just do :info YourTypeClass in GHCI.
2. So typing :info Num will show which functions the typeclass defines and it will give you a list of the types in the typeclass.
3. :info works for types and type constructors too. If you do :info Maybe, it will show you all the typeclasses that Maybe is an instance of.
4. Also :info can show you the type declaration of a function. I think that's pretty cool.
-}

--implementing a YesNo type
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance (YesNo a) => YesNo (Maybe a) where
    yesno (Just x) = yesno x
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesno $ length [] --False
yesno "haha" --True
yesno "" --False
yesno $ Just 0 --True
yesno True --True
yesno EmptyTree --False
yesno [] --False
yesno [0,0,0] --True
:t yesno --yesno :: (YesNo a) => a -> Bool

--mimic an if-statement
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf test y n = if yesno test then y else n

yesnoIf [] "YEAH!" "NO!" --"NO!"
yesnoIf [2,3,4] "YEAH!" "NO!" --"YEAH!"
yesnoIf True "YEAH!" "NO!" --"YEAH!"
yesnoIf (Just 500) "YEAH!" "NO!" --"YEAH!"
yesnoIf Nothing "YEAH!" "NO!" --"NO!"

--Functors!!!!!!!!!!!!! Functor is a typeclass for types that can be mapped over
{-Functor takes a type variable f and applies it as a function that returns type (f a) and (f b)-}
class Functor f where
    fmap :: (a -> b) -> f a -> f b  -- not logic, instead (f a) returns a type, this is used to define the types
-- defines fmap with no default implementation
--Functor is different from Eq implementation above, or Maybe, because those are type constructors that take a concrete type (i.e. Maybe a), but Functor takes a type constructor itself that takes one parameter

--remember, from Data.List
:t Data.List.Map -- map :: (a -> b) -> [a] -> [b]

--we can define fmap for Lists like so...
instance Functor [] where
    fmap = map
-- we didn't write `instance Functor [a] where` because `[a]` is a concrete type, and we want to define how to map over any list, not just a list of [a]'s

fmap (*2) [1..3] --[2,4,6]
map (*2) [1..3] --[2,4,6]

{-HOW DOES MAYBE IMPLEMENT THE FUNCTOR INTERFACE?-}
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")  -- Just "Something serious. HEY GUYS IM INSIDE THE JUST"
fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing --Nothing
fmap (*2) (Just 200) --Just 400
fmap (*2) Nothing --Nothing

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

fmap (*2) EmptyTree --EmptyTree
fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7]) --Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree

-- what about Either? Remember how it is a Typeclass constructor that takes 2 values? Well, we can partially apply Typeclass constructors.
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

data Either a b = Left a | Right b  -- remember how Either is defined?

instance Functor Either where... --wouldn't work, because the typeclass of Functor expects a type constructor that takes only one input
--We've seen that type constructors can be partially applied (Either String is a type that takes one type and produces a concrete type, like Either String Int), just like functions can.

{- Kinds

- Values like 3, "yeah", and takeWhile have a type
- types themselves have "kinds", which is a type of type

-}

:k Int -- Int :: *
-- * means the type is a CONCRETE type; it is a type constructor that takes no parameters
-- values can only have types that are CONCRETE

:k Maybe -- Maybe :: * -> *
-- Maybe type constructor takes one CONCRETE type and returns a new CONCRETE type, like Maybe Int

:k Maybe Int -- Maybe  Int  ::  *
-- Maybe Int is a CONCRETE type

--similar to testing the type of a value
:t isUpper -- Char -> Bool
:t isUpper 'A' -- Bool
-- both of these values have a kind of *

:k Either -- Either :: * -> * -> *
-- takes two CONCRETE types as type parameters to produce a new CONCRETE type. They can be partially applied.

:k Either String -- Either String :: * -> *
:k Either String Int -- Either String Int :: *

{-remember Functor?-}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
{-type constructors that want to be friends with Functor must return kind (* -> *) -}

{-now consider this-}
class Tofu t where
    tofu :: j a -> t a j
{-
(:k (j a)) is * (because it is an input to fn tofu)
so (:k a) is * and (:k j) is * -> *
so (:k t) is * -> (* -> *) -> *

OK, so let's make a type with a kind of * -> (* -> *) -> *. Here's one way of going about it.
-}

data Frank a b = Frank {frankField :: b a} deriving (Show)

{-
fields of records are meant to hold values, so a Frank a b means that
- frankField holds a concrete type
- thus b must be * -> *, and
- a must be *

thus Frank is a type that takes * -> (* -> *) and returns *, i.e. :k Frank is * -> (* -> *) -> *
-}

:t Frank {frankField = Just "HAHA"} --Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
:t Frank {frankField = Node 'a' EmptyTree EmptyTree} --Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
:t Frank {frankField = "YES"} --Frank  {Frankfield  =  "YES" }  ::  Frank  char  []

--Making Frank an instance of Tofu is pretty simple
instance Tofu Frank where
    tofu x = Frank x

tofu (Just 'a') :: Frank Char Maybe --Frank {frankField = Just 'a'}
tofu ["HELLO"] :: Frank [Char] [] --Frank {frankField = ["HELLO"]}

{- I/O, separating pure from impure parts in our programs -}

-- example single-line Haskell program, helloworld.hs
main = putStrLn "hello, world"

-- terminal commands to compile
$ ghc --make helloworld
[1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )
Linking helloworld ...
$ ./helloworld
hello, world

--another way to compile and run the file
$ runhaskell helloworld.hs

--putStrLn has a type
:t putStrLn -- putStrLen :: String -> IO ()
:t putStrLn "hello, world" -- putStrLn "hello, world" :: IO ()
--------------------------------------------------------    ^ empty tuple

--we can use _do syntax_ to glue together several I/O actions into one
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hey " ++ name ++ ", you rock!"

:t getLine -- getLine :: IO String
-- thus
name <- getLine
-- must get input from IO, then take the resulting String and bind it to name

:t return -- a -> IO a
main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b
-- return in Haskell doesn't end the execution, instead it produces an IO that can be bound

--putChar prints a char to the terminal
main = do   putChar 't'
            putChar 'e'
            putChar 'h'

$ runhaskell putchar_test.hs
teh

-- we can recurse on functions that perform IO, creating a putStr
-- do statements return an IO that yields the value from the final statement
putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
    putChar x
    putStr xs

-- and even a generic print fn
main = do   print True
            print 2
            print "haha"
            print 3.2
            print [3,4,3]

$ runhaskell print_test.hs
True
2
"haha"
3.2
[3,4,3]

--the when fn is provided in Control.Monad
import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main

--sequence is also provided in Control.Monad
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

sequence $ map print [1,2,3,4,5]
1
2
3
4
5
[(),(),(),(),()]

--because sequence $ map is so common, we have built-in mapM fn's from Control.Monad
mapM print [1,2,3]
1
2
3
[(),(),()]

mapM_ print [1,2,3]
1
2
3

--we can even use Control.Monad.forever

import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

--forM
import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors

{-files and streams-}
-- getContents!!!!!!!!!!
-- getContents is IO lazy, meaning it reads in bytes as a stream instead of buffering everything to memory
import Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)

$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS

--filter for short lines
main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result

--This pattern of getting some string from the input, transforming it with a function and then outputting that is so common that there exists a function which makes that even easier, called interact

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result

-- composing this in one line
main = interact $ unlines . filter ((<10) . length) . lines

--read from a file with `openFile` and hGetContents
import System.IO

main = do
    handle <- openFile "file.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

:t openFile -- openFile :: FilePath -> IOMode -> IO Handle
:t FilePath -- String -- FilePath is just a type synonym for String, simply defined as:
type FilePath = String
:info ReadMode --ReadMode is a value of IOMode
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
--hGetContents and hClose take a file handle as input

--could have used the withFile function, instead
:t withFile -- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

import System.IO

main = do
    withFile "file.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

--we can writ eour own withFile'

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

--similar to hGetContents, there are several fn's that are used to pull file contents
hGetLine, hPutStr, hPutStrLn, hGetChar
--these exist as parallels to getLine, putStr, etc... only instead of working with stdin/stdout, they work with file handles

:t readFile --readFile :: FilePath -> IO String
--similar to previos example above the reads file.txt withFile()
import System.IO

main = do
    contents <- readFile "file.txt"
    putStr contents

--there's also writeFile
:t writeFile --writeFile :: FilePath -> String -> IO ()

import System.IO
import Data.Char

main = do
    contents <- readFile "file.txt"
    writeFile "caps.txt" (map toUpper contents)

--can also appendFile
import System.IO

main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

--file I/O is lazy
main = do
    withFile "something.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

--remember how the above reads only bits at a time? is actually like connecting a pipe from the file to the output. Just like you can think of lists as streams, you can also think of files as streams. This will read one line at a time and print it out to the terminal as it goes along. So you may be asking, how wide is this pipe then? How often will the disk be accessed? Well, for text files, the default buffering is line-buffering usually. For binary files, the default buffering is usually block-buffering.
--You can control the buffering, if you wish
main = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
--Reading files in bigger chunks can help if we want to minimize disk access or when our file is actually a slow network resource.

--remove an item from todo.txt
import System.IO -- gives us openTempFile
import System.Directory -- could have used getCurrentDirectory but just passed '.' as file path, same thing in Unix; removeFile, renameFile
import Data.List -- gives us !! and delete

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"

import System.Environment -- gives us getArgs and getProgName
:t getArgs -- getArgs :: IO [String]
:t getProgName -- getProgName :: IO String

import System.Environment
import Data.List

main = do
   args <- getArgs
   progName <- getProgName
   putStrLn "The arguments are:"
   mapM putStrLn args
   putStrLn "The program name is:"
   putStrLn progName

$ ./arg-test first second w00t "multi word arg"
The arguments are:
first
second
w00t
multi word arg
The program name is:
arg-test