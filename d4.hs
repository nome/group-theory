module Main where
import GroupTheory
import Data.List
import Data.Function

-- D_4 as a subset of the symmetric group S_4
--data Vertex = A | B | C | D deriving (Eq, Ord, Enum, Bounded, Show)
--d4 = [(cycle4 A B C D), (cycle2 A C)]

-- D_4 as a subset of the free group F_2
data Generator = B | C deriving (Eq, Ord, Enum, Bounded, Show)
instance Presentation Generator where
	relators = [ power (gen C) 4, power (gen B) 2, power (gen C ° gen B) 2 ]
d4 = map subjugate [ gen B, gen C ]

{-
 - the stuff below this point is independent of the representation of D_4 chosen above
 -}

-- Sort subgroup elements by whatever ordering is defined for the chosen presentation in
-- GroupTheory, and subgroups by their size.
sortSubgroups = sortBy (compare `on` length) . (map sort)

-- simple test of subsets for invariance
isInvariantIn group subset = all (\cc -> intersect cc subset == cc || null (intersect cc subset)) (conjugacyClasses group)

main = do
	putStrLn $ "D_4 = " ++ show (sort $ generatedGroup d4)
	putStrLn "Conjugacy classes:"; mapM_ print $ conjugacyClasses d4
	putStrLn "Subgroups:"; mapM_ print $ sortSubgroups $ subgroups d4
	putStrLn "Invariant subgroups:"; mapM_ print $ sortSubgroups $ filter (isInvariantIn d4) (subgroups d4)
