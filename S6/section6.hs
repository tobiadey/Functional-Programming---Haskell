module Section6 where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


test::Int
test = 20


-- simple set interactions

--removes duplicates in a list
-- In terminal type: unique [1,2,2,2,3,3,4,5,6,6,62]
unique :: Ord a => [a] -> [a]
unique xs = Set.elems (Set.fromList xs)

--retruns a map of pairs of 
--each value and number of times it occurs in a list
-- In terminal type: unique [1,2,2,2,3,3,4,5,6,6,62]
frequencyMap :: Ord a => [a] -> Map a Int 
frequencyMap xs = Map.unionsWith (+) [Map.singleton x 1 | x <- xs]