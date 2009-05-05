module GroupTheory where
import Data.List

{-
 - Some (very) basic group theoretical functions.
 -
 - The intention here is to exploit Haskell's nice syntax and type system in order to provide
 - a clear, readable description of the concepts involved. The fact that you can actually run
 - this description on a computer and explore what it does is more or less a bonus. Particularly,
 - little to no effort has been invested in making the code efficient.
 -
 - Usage example: The dihedral group of order 8.
 -
 - module Main where
 - import GroupTheory
 -
 - data Vertex = A | B | C | D deriving (Eq, Enum, Bounded, Show)
 - d4 = GeneratedGroup [(cycle4 A B C D), (cycle2 A C)]
 - main = do
 - 	print $ elements d4
 - 	print $ conjugacyClasses d4
 - 	print $ subgroups d4
 -}

{------------------------------------------------------------------------------
 - some utility functions not specific to group theory
 ------------------------------------------------------------------------------}
 
 stabilize :: Eq a => (a -> a) -> a -> a
 stabilize f x = if next == x then x else stabilize f next
 	where
		next = f x

{-
 - Moderately similar to iterate, but allows the function to generate multiple elements in one go
 - and also takes multiple start values. Returns a list of all values that can be generated by
 - repeated application of f to elements of the input list. In contrast to iterate, setIterate
 - terminates when f does not produce any new values.
 -}
setIterate :: Eq a => (a -> [a]) -> [a] -> [a]
setIterate f list = iter list list
	where
		iter accum [] = accum
		iter accum todo = iter (newElements ++ accum) newElements where
			generatedElements = foldl (\a x -> (union a) $ f x) [] todo
			newElements = generatedElements \\ accum

{-
 - What the name says - swap the (first two) arguments of a function.
 -}
swapArgs :: (a -> b -> c) -> (b -> a -> c)
swapArgs f x y = f y x

{-
 - Test two lists for equality, assuming they represent sets (i.e. test whether they contain the
 - same set of elements, in any order).
 -}
setEqual :: Eq a => [a] -> [a] -> Bool
setEqual l1 l2 = (length l1) == (length l2) && null (nub l1 \\ l2)

{------------------------------------------------------------------------------
 - basic group theory definitions and functions
 ------------------------------------------------------------------------------}

{-
 - Of course, the point here is that instances of this class have to obey the group axioms;
 - i.e. ° has to be associative, g°neutral==g and g°(inverse g)==neutral for all possible values.
 - Otherwise, using the instance will likely yield unexpected results.
 -}
class Group a where
	(°) :: a -> a -> a
	neutral :: a
	inverse :: a -> a

{-
 - Raise a group element to an integer power (using the group axioms).
 -}
power :: Group a => a -> Int -> a
power g n
	| n > 0  = (iterate (g°) g) !! (n-1)
	| n == 0 = neutral
	| n < 0  = power (inverse g) (-n)

{-
 - Conjugate of p with respect to q.
 -}
conjugate :: Group a => a -> a -> a
conjugate q p = q ° p ° inverse q

{-
 - Commutator of p and q
 -}
commutator :: Group a => a -> a -> a
commutator p q = p ° q ° inverse (q ° p)

{------------------------------------------------------------------------------
 - finitely generated groups
 - (actually, most of the functions here assume that the group itself is finite)
 ------------------------------------------------------------------------------}

{-
 - Basically, a generated group is defined by a list of generators, which are required to be of a
 - type of class Group.
 -}
data (Group a, Eq a) => GeneratedGroup a = GeneratedGroup [a]

{-
 - Unpack list of generators from GeneratedGroup constructor.
 -}
generators :: (Group a, Eq a) => GeneratedGroup a -> [a]
generators (GeneratedGroup gens) = gens

{-
 - Compute a list of all distinct elements that can be built from the generators, i.e. the set of
 - the group's elements.
 -}
elements :: (Group a, Eq a) => GeneratedGroup a -> [a]
elements group = union [neutral] $ setIterate (\h -> map (h°) base) base
	where
		base = union (generators group) (map inverse (generators group))

{-
 - Given a generated group and an element thereof, return a list of all elements conjugated to it;
 - i.e. its conjugacy class.
 -}
conjugacyClass :: (Group a, Eq a) => GeneratedGroup a -> a -> [a]
conjugacyClass group g = setIterate (\h -> map (swapArgs conjugate h) base) [g]
	where
		base = union (generators group) (map inverse (generators group))

{-
 - Compute all conjugacy classes of the given generated group.
 -}
conjugacyClasses :: (Group a, Eq a) => GeneratedGroup a -> [[a]]
conjugacyClasses group = classify (elements group) where
	classify [] = []
	classify (x:xs) = let c = conjugacyClass group x in c:(classify (xs \\ c))

{-
 - Compute all subgroups of the given generated group.
 -}
subgroups :: (Group a, Eq a) => GeneratedGroup a -> [[a]]
subgroups group = nubBy setEqual [ elements $ GeneratedGroup (concat minSubGroups) | minSubGroups <- subsequences minimalSubgroups ]
	where
		minimalSubgroups = nubBy setEqual [ elements $ GeneratedGroup [g] | g <- elements group ]

{-------------------------------------------------------------
 - permutation groups - symmetric groups and their subgroups
 -------------------------------------------------------------}

{-
 - We use association lists for representing permutations, because that's the conceptually simplest
 - way (i.e. should be understandable with very basic knowledge of Haskell). The slightly clumsy
 - lookup syntax is worked around by adding a function "permute" to do this.
 -}
newtype Permutation a = Permutation [(a,a)]

{-
 - Applying a permutation to the underlying set just means looking up the elements in the
 - association list. It is generally assumed that the association list is complete, i.e. contains
 - all possible values of the underlying type; but for completeness, we assume here that an element
 - without association should be left invariant.
 -}
permute :: Eq a => Permutation a -> a -> a
permute (Permutation p) x = case lookup x p of
	Just y -> y
	Nothing -> x

{-
 - Two permutations are equal iff they have the same set of associations. This one does depend on
 - the association lists being complete (else different association lists describing the same
 - permutation may compare unequal).
 -}
instance Eq a => Eq (Permutation a) where
	(Permutation p1) == (Permutation p2) = setEqual p1 p2

{-
 - The symmetric group of a bounded, enumerable type (=> finite set).
 - While the list of values to operate on is implicit in the representation chosen for permutations,
 - we need explicit bounds and enumerability in order to construct the neutral element.
 -}
instance (Eq a, Enum a, Bounded a) => Group (Permutation a) where
	p1 ° p2 = Permutation $ [(x, permute p1 $ permute p2 x) | x <- enumFromTo minBound maxBound]
	neutral = Permutation $ [(x,x) | x <- enumFromTo minBound maxBound]
	inverse (Permutation p) = Permutation $ map (\(a,b) -> (b,a)) p

{-
 - Orbit of x under the cyclic group generated by f.
 - f is assumed to describe the action of an element of a finite group on type a; otherwise, the
 - result may be an infinite list (and technically the term "orbit" isn't usually defined for
 - arbitrary f).
 -}
orbit :: Eq a => (a -> a) -> a -> [a]
orbit f x = x : (takeWhile (/= x) $ iterate f (f x))

{-
 - Disjoint cycle decomposition of permutations.
 -}
cycles :: (Eq a, Enum a, Bounded a) => Permutation a -> [[a]]
cycles p = cyclesContaining (enumFromTo minBound maxBound)
	where
		cyclesContaining [] = []
		cyclesContaining (x:xs) = let o = orbit (permute p) x in o:(cyclesContaining (xs\\o))

{-
 - Use cyclic notation for displaying permutations (omitting 1-cycles).
 -}
instance (Eq a, Enum a, Bounded a, Show a) => Show (Permutation a) where
	show p | null nontrivialCycles = "()"
	       | otherwise = concat $ map (("("++) . (++")") . showCycle) nontrivialCycles
		where
			nontrivialCycles = filter ((>1).length) $ cycles p
			showCycle (x:[]) = show x
			showCycle (x:rest) = show x ++ " " ++ showCycle rest

{-
 - Make permutation from a bijection on the underlying set.
 -}
permutationFromBijection :: (Enum a, Bounded a) => (a -> a) -> Permutation a
permutationFromBijection f = Permutation [(x, f x) | x <- enumFromTo minBound maxBound]

{-
 - Make permutation from a two cycle (convenience function).
 -}
cycle2 :: (Eq a, Enum a, Bounded a) => a -> a -> Permutation a
cycle2 a b = permutationFromBijection act
	where act i	| i == a = b
				| i == b = a
				| otherwise = i

{-
 - Make permutation from a three cycle (convenience function).
 -}
cycle3 :: (Eq a, Enum a, Bounded a) => a -> a -> a -> Permutation a
cycle3 a b c = permutationFromBijection act
	where act i	| i == a = b
				| i == b = c
				| i == c = a
				| otherwise = i

{-
 - Make permutation from a four cycle (convenience function).
 -}
cycle4 :: (Eq a, Enum a, Bounded a) => a -> a -> a -> a -> Permutation a
cycle4 a b c d = permutationFromBijection act
	where act i	| i == a = b
				| i == b = c
				| i == c = d
				| i == d = a
				| otherwise = i

{-
 - Make permutation from a five cycle (convenience function).
 -}
cycle5 :: (Eq a, Enum a, Bounded a) => a -> a -> a -> a -> a -> Permutation a
cycle5 a b c d e = permutationFromBijection act
	where act i	| i == a = b
				| i == b = c
				| i == c = d
				| i == d = e
				| i == e = a
				| otherwise = i

{-------------------------------------------------------------
 - finitely presented groups - as subgroups of a free group
 -------------------------------------------------------------}
 
{-
 - Elements of a free group are words in the generators and their inverses, modelled as a list of
 - Left x (representing generator x) and Right x (representing x^-1) values.
 -}
data Word a = Word [Either a a]

{-
 - A presentation is specified by a list of generators and a list of relators.
 -}
data PresentedGroup a = PresentedGroup [a] [Word a]

type RewriteRules a = [(Word a, Word a)]

{-
 - Apply a set of rewrite rules to a word until none of them matches any more. Can loop forever
 - on some inputs.
 -}
applyRewriteRules :: Eq a => RewriteRules a -> Word a -> Word a
applyRewriteRules allRules word = iter allRules word
	where
		iter :: Eq a => RewriteRules a -> Word a -> Word a
		iter [] word = word
		iter rules@(r:rs) word = case tryRewrite r word of
			Just word' -> iter allRules word' -- try all rules again on changed word
			Nothing -> iter rs word -- try rest of rules on unchanged word

		-- try to apply one rule to its first occurance, if any, in input
		tryRewrite :: Eq a => (Word a, Word a) -> Word a -> Maybe (Word a)
		tryRewrite _ (Word []) = Nothing
		tryRewrite rule@(Word left, Word right) (Word input@(i:is)) = do
			if left `isPrefixOf` input then
				return $ Word $ right ++ drop (length left) input
				else
					result <- tryRewrite rule (Word is)
					return $ Word (i:result)

{-
 - Use Knuth-Bendix algorithm to extend the rewrite rules given by relators and the inverses to a
 - confluent term rewriting system. This is not guaranteed to succeed (read: terminate in finite
 - time) for any set of relators; in fact, it can be proven that there are groups with undecidable
 - word problem; i.e., there's no other algorithm which does the same task for every possible input.
 -}
knuthBendixRewrite :: Eq a => PresentedGroup a -> RewriteRules a
knuthBendixRewrite (PresentedGroup generators relators) = step $ inverseRules ++ relatorRules
	where
		inverseRules = map (\x -> (PresentedGroupElement [Left x, Right x], PresentedGroupElement [])) generators
		relatorRules = map (\r -> (r, PresentedGroupElement [])) relators
		reduce rules = 

-- http://en.wikipedia.org/wiki/Knuth–Bendix_completion_algorithm  (-> GAP, kbsemi.gi)
-- http://en.wikipedia.org/wiki/Todd–Coxeter_algorithm
