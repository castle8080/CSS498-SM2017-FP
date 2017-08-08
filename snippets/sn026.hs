import Data.List

map1 f xs = foldl (\xs' x -> (f x) : xs') [] xs

map2 f xs = foldr (\x xs' -> (f x) : xs') [] xs



