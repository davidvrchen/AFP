import Data.Function (fix)
import Data.List (unfoldr)

-- We start with four definitions of nats,
-- an infinite list of all natural numbers
nats :: [Int]
-- ^ Infinite list of natural numbers
-- via built in list comprehension
nats = [0 ..]

natsRec :: [Int]
-- ^ Infinite list of natural numbers
--  via recursion
natsRec = 0 : map (+ 1) natsRec

natsAna :: [Int]
-- ^ Infinite list of natural numbers
--  via anamorphism
natsAna = unfoldr (\x -> Just (x, x + 1)) 0

natsFix :: [Int]
-- ^ Infinite list of natural numbers
--  as fix point solution of algebraic equation
natsFix = fix (\l -> 0 : map (+ 1) l)

-- Next up are three definitions of mymap
mymapRec ::
  (x -> y) ->
  [x] ->
  [y]
-- ^ Recursive map from scratch
mymapRec f [] = []
mymapRec f (x : xs) = f x : mymapRec f xs

mymapCata ::
  (x -> y) ->
  [x] ->
  [y]
-- ^ Map via foldr aka as catamorphism
mymapCata f = foldr (\x xs -> f x : xs) []

mymapFix ::
  (x -> y) ->
  [x] ->
  [y]
-- ^ Map as fixed point solution of equation
mymapFix =
  fix
    ( \m f xs -> case xs of
        [] -> []
        x : xs -> f x : m f xs
    )

-- Lastly we look at the definition of ftake,
-- i.e. composition of flip and take
ftakeRec :: [x] -> Int -> [x]
-- ^ Recursive definition of ftake
ftakeRec xs 0 = []
ftakeRec [] n = []
ftakeRec (x : xs) n = ftakeRec xs (n -1) ++ [x]

ftakeCata :: [x] -> Int -> [x]
-- ^ ftake as catamorphism
ftakeCata =
  foldr
    ( \a as l -> case l of
        0 -> []
        l -> as (l -1) ++ [a]
    )
    (const [])

ftakeFix :: [x] -> Int -> [x]
-- ^ ftake as fixed point solution of equation
ftakeFix =
  fix
    ( \t (x : xs) n -> case n of
        0 -> []
        n -> t xs (n -1) ++ [x]
    )
