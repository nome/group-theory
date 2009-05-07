module Main where
import GroupTheory
import Text.ParserCombinators.Parsec
import System.Console.Readline
import System.Exit

{-
 - basic definitions for Rubik's cube
 -}

data Facet =	Flu | Fl | Fld | Fru | Fr | Frd | Fu | Fd |
					Blu | Bl | Bld | Bru | Br | Brd | Bu | Bd |
					Lfu | Lf | Lfd | Lbu | Lb | Lbd | Lu | Ld |
					Rfu | Rf | Rfd | Rbu | Rb | Rbd | Ru | Rd |
					Ufl | Uf | Ufr | Ubl | Ub | Ubr | Ul | Ur |
					Dfl | Df | Dfr | Dbl | Db | Dbr | Dl | Dr
					deriving (Eq, Ord, Enum, Bounded, Show)

type Move = Permutation Facet

f = (cycle4 Flu Fru Frd Fld) ° (cycle4 Fu Fr Fd Fl) ° (cycle4 Ufl Rfu Dfr Lfd) ° (cycle4 Uf Rf Df Lf) ° (cycle4 Ufr Rfd Dfl Lfu)
b = (cycle4 Bru Blu Bld Brd) ° (cycle4 Bu Bl Bd Br) ° (cycle4 Ubl Lbd Dbr Rbu) ° (cycle4 Ub Lb Db Rb) ° (cycle4 Ubr Lbu Dbl Rbd)
l = (cycle4 Lbu Lfu Lfd Lbd) ° (cycle4 Lu Lf Ld Lb) ° (cycle4 Ufl Fld Dbl Blu) ° (cycle4 Ul Fl Dl Bl) ° (cycle4 Ubl Flu Dfl Bld)
r = (cycle4 Rfu Rbu Rbd Rfd) ° (cycle4 Ru Rb Rd Rf) ° (cycle4 Ufr Bru Dbr Frd) ° (cycle4 Ur Br Dr Fr) ° (cycle4 Ubr Brd Dfr Fru)
u = (cycle4 Ubl Ubr Ufr Ufl) ° (cycle4 Ub Ur Uf Ul) ° (cycle4 Blu Rbu Fru Lfu) ° (cycle4 Bu Ru Fu Lu) ° (cycle4 Bru Rfu Flu Lbu)
d = (cycle4 Dfl Dfr Dbr Dbl) ° (cycle4 Df Dr Db Dl) ° (cycle4 Fld Rfd Brd Lbd) ° (cycle4 Fd Rd Bd Ld) ° (cycle4 Frd Rbd Bld Lfd)

parseModifier :: GenParser Char st (Move -> Move)
parseModifier =	(char '\''		>> return inverse)												<|>
						((many1 digit	>>= return.(swapArgs power).read)	<?> "exponent")	<|>
						(char '^'		>> parseMove >>= return.conjugate)							<|>
						(return id)

parseAtomic :: GenParser Char st Move
parseAtomic = do
	let parseF = char 'f' >> return f
	let parseB = char 'b' >> return b
	let parseL = char 'l' >> return l
	let parseR = char 'r' >> return r
	let parseU = char 'u' >> return u
	let parseD = char 'd' >> return d
	atom <- parseF <|> parseB <|> parseL <|> parseR <|> parseU <|> parseD
	modifier <- parseModifier
	return $ modifier atom

parseParens :: GenParser Char st Move
parseParens = do
	char '('
	content <- parseWord
	char ')'
	modifier <- parseModifier
	return $ modifier $ content

parseCommutator :: GenParser Char st Move
parseCommutator = do
	char '['
	a <- parseWord
	char ','
	b <- parseWord
	char ']'
	mod <- parseModifier
	return $ mod $ commutator a b

parseMove :: GenParser Char st Move
parseMove = parseAtomic <|> parseParens <|> parseCommutator <?> "move"

parseWord :: GenParser Char st Move
parseWord = many1 parseMove >>= (return . (foldr (swapArgs (°)) neutral))

forever a = a >> forever a

main = do
	initialize
	forever (readline "rubik> " >>= command)
	where
	command Nothing = putStrLn ""
	command (Just "quit") = exitWith ExitSuccess
	command (Just line) = do
		let lhs = takeWhile (/= '=') line
		let rhs = dropWhile (/= '=') line
		let cmdDecompose = case parse parseWord "<input>" line of
			Left err -> putStrLn $ "Invalid expression: " ++ show err
			Right result -> putStr "-> " >> print result
		let cmdEqual = case parse parseWord "lhs" lhs of
			Left err -> putStrLn $ "Invalid expression: " ++ show err
			Right result1 -> case parse parseWord "rhs" (tail rhs) of
				Left err -> putStrLn $ "Invalid expression: " ++ show err
				Right result2 -> print (result1 == result2)

		if null rhs then cmdDecompose else cmdEqual
		addHistory line
