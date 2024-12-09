import System.IO
import Control.Monad
import Data.List
import qualified Data.Map as Map

parseLine :: String -> (Int, Int)
parseLine line = let [x, y] = map read (words line) in (x, y)

part1 :: [(Int, Int)] -> Int
part1 pairs = sum $ zipWith (\a b -> abs (a - b)) xs ys
    where
	(xs, ys) = (sort $ map fst pairs, sort $ map snd pairs)

part2 :: [(Int, Int)] -> Int
part2 pairs = sum $ zipWith (*) (Map.elems intersection) (Map.keys intersection)
    where 
    	(map1, map2) = (toCounter $ map fst pairs, toCounter $ map snd pairs)
	intersection = Map.intersectionWith (*) map1 map2

toCounter :: [Int] -> Map.Map Int Int
toCounter xs = foldr count Map.empty xs
    where
	count x = Map.insertWith (+) x 1

main :: IO ()
main = do
	contents <- readFile "input"
	let pairs = map parseLine (lines contents)
	putStrLn $ "Part 1: " ++ show (part1 pairs)
	putStrLn $ "Part 2: " ++ show (part2 pairs)

