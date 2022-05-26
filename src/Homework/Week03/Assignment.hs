module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skip :: Int -> [a] -> [a]
skip n =
  let f [] = []
      f (b:bs) = b : (f (drop (n-1) bs))
  in f . (drop (n-1))

skips :: [a] -> [[a]]
skips as = map (flip skip as) [1..(length as)]

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima =
  let f ms (a:b:c:ns)
        | b > a && b > c = f (b:ms) (b:c:ns)
        | otherwise      = f ms (b:c:ns)
      f ms _ = ms
  in reverse . (f [])

-- #3
count :: [Integer] -> Integer -> Int
count ns n = (length . filter (==n)) ns

row :: [Integer] -> Int -> [Bool]
row ns n = map ((>=n) . count ns) [0..9]

rows :: [Integer] -> [[Bool]]
rows ns = takeWhile (any id) $ map (row ns) [1..]

histRow :: [Bool] -> String
histRow =
  let f True  = '*'
      f False = ' '
  in map f

hist :: [Integer] -> String
hist = unlines . map histRow . reverse . rows

histogram :: [Integer] -> String
histogram ns = (hist ns) ++ "==========\n0123456789\n"
