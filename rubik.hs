{-
 - Copyright (C) 2009 by Knut Franke
 -
 - This program is free software; you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation; either version 2 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program; if not, write to the Free Software
 - Foundation, Inc., 51 Franklin Street, Fifth Floor,
 - Boston, MA  02110-1301  USA
 -}

module Main where
import GroupTheory
import Text.ParserCombinators.Parsec
import System.Console.Readline
import System.Exit

{-
 - This is a small command-line utility built on top of the GroupTheory module
 - for the purpose of studying the Rubik's cube group. It can evaluate moves
 - given in an extended Singmaster notation, display the result in disjoint
 - cycle notation and compare moves for equality.
 -
 - Apart from GroupTheory, it depends on parsec and readline, both of which
 - are not distributed with GHC any more but are easily obtainable as part of
 - many Linux distributions or via http://hackage.haskell.org.
 -}

{------------------------------------------------------------------------------
 - basic definitions for Rubik's cube
 ------------------------------------------------------------------------------}

data Facet =	Flu | Fl | Fld | Fru | Fr | Frd | Fu | Fd |
					Blu | Bl | Bld | Bru | Br | Brd | Bu | Bd |
					Lfu | Lf | Lfd | Lbu | Lb | Lbd | Lu | Ld |
					Rfu | Rf | Rfd | Rbu | Rb | Rbd | Ru | Rd |
					Ufl | Uf | Ufr | Ubl | Ub | Ubr | Ul | Ur |
					Dfl | Df | Dfr | Dbl | Db | Dbr | Dl | Dr
					deriving (Eq, Ord, Enum, Bounded, Show)

type Move = Permutation Facet

-- the primitive moves (Singmaster notation)
f = (cycle4 Flu Fru Frd Fld) ° (cycle4 Fu Fr Fd Fl) ° (cycle4 Ufl Rfu Dfr Lfd) ° (cycle4 Uf Rf Df Lf) ° (cycle4 Ufr Rfd Dfl Lfu)
b = (cycle4 Bru Blu Bld Brd) ° (cycle4 Bu Bl Bd Br) ° (cycle4 Ubl Lbd Dbr Rbu) ° (cycle4 Ub Lb Db Rb) ° (cycle4 Ubr Lbu Dbl Rbd)
l = (cycle4 Lbu Lfu Lfd Lbd) ° (cycle4 Lu Lf Ld Lb) ° (cycle4 Ufl Fld Dbl Blu) ° (cycle4 Ul Fl Dl Bl) ° (cycle4 Ubl Flu Dfl Bld)
r = (cycle4 Rfu Rbu Rbd Rfd) ° (cycle4 Ru Rb Rd Rf) ° (cycle4 Ufr Bru Dbr Frd) ° (cycle4 Ur Br Dr Fr) ° (cycle4 Ubr Brd Dfr Fru)
u = (cycle4 Ubl Ubr Ufr Ufl) ° (cycle4 Ub Ur Uf Ul) ° (cycle4 Blu Rbu Fru Lfu) ° (cycle4 Bu Ru Fu Lu) ° (cycle4 Bru Rfu Flu Lbu)
d = (cycle4 Dfl Dfr Dbr Dbl) ° (cycle4 Df Dr Db Dl) ° (cycle4 Fld Rfd Brd Lbd) ° (cycle4 Fd Rd Bd Ld) ° (cycle4 Frd Rbd Bld Lfd)

{------------------------------------------------------------------------------
 - Parser for composite move expressions. (extended Singmaster notation)
 -
 - Understands the primitives fblrud, x' for the inverse of x, postfix numbers
 - (e.g. x4) as exponents, x^y for the conjugate of x with respect to y and
 - commutators of the form [x,y]. Subexpressions can be grouped using
 - parentheses.
 ------------------------------------------------------------------------------}

-- move modifiers: inverses, exponents and conjugates
parseModifier :: GenParser Char st (Move -> Move)
parseModifier =	(char '\''		>> return inverse)												<|>
						((many1 digit	>>= return.(swapArgs power).read)	<?> "exponent")	<|>
						(char '^'		>> parseMove >>= return.conjugate)							<|>
						(return id)

-- atoms: fblrud, optionally with modifiers
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

-- subexpression enclosed by parentheses, optionally with modifiers
parseParens :: GenParser Char st Move
parseParens = do
	char '('
	content <- parseWord
	char ')'
	modifier <- parseModifier
	return $ modifier $ content

-- commutator of two expressions, optionally with modifier
parseCommutator :: GenParser Char st Move
parseCommutator = do
	char '['
	a <- parseWord
	char ','
	b <- parseWord
	char ']'
	mod <- parseModifier
	return $ mod $ commutator a b

-- a single atom, parenthesized group or commutator
parseMove :: GenParser Char st Move
parseMove = parseAtomic <|> parseParens <|> parseCommutator <?> "move"

-- one or more atoms, parenthesized groups or commutators
parseWord :: GenParser Char st Move
parseWord = many1 parseMove >>= (return . (foldr (swapArgs (°)) neutral))

{-
 - Main loop
 -
 - Supports evaluation of expressions, equality tests ("x=y") and the command "quit".
 -}

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
