{-# LANGUAGE QuasiQuotes, FlexibleContexts, PackageImports #-}

module Cons (
	Cons(..),
	isDouble,
	getDouble,
	Atom(..),
	parse,
	Environment
) where

import Text.Peggy (peggy, space, defaultDelimiter, parseString)
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.State

type Environment = [(String, Atom)]

data Cons = Cons { car :: Cons, cdr :: Cons } | Atom Atom

instance Show Cons where
	show c@(Cons _ _) = "(" ++ showCons c ++ ")"
	show (Atom a) = show a

showCons :: Cons -> String
showCons (Cons a d@(Cons _ _)) = show a ++ " " ++ showCons d
showCons (Cons a (Atom Null)) = show a
showCons (Cons a d) = show a ++ " . " ++ show d

instance Read Cons where
	readsPrec 0 inp = case parse inp of
		Just r -> [(r, "")]
		Nothing -> []

data Atom
	= Undef
	| Null
	| F
	| T
	| Define
	| Lambda
	| Cond
	| Variable String
	| Int { getInt :: Int }
	| Double Double
	| Function ([Atom] -> ErrorT String (StateT Environment IO) Atom)
	| Clojure Environment [String] Cons

instance Show Atom where
	show Undef = "#<undef>"
	show Null = "()"
	show T = "#t"
	show F = "#f"
	show Define = "#<syntax define>"
	show Lambda = "#<syntax lambda>"
	show Cond = "#<syntax cond>"
	show (Int n) = show n
	show (Double d) = show d
	show (Function _) = "(Function _)"
	show (Variable v) = v
	show (Clojure _ _ _) = "#<clojure _>"

isDouble (Double _) = True
isDouble _ = False

getDouble :: Atom -> Double
getDouble (Int n) = fromIntegral n
getDouble (Double d) = d

parse :: String -> Maybe Cons
parse = either (const Nothing) Just . parseString cons ""

[peggy|

cons :: Cons
	= '(' cons ' '* '.' ' '* cons ')'	{ Cons $1 $4 }
	/ '(' list ')'				{ $1 }
	/ atom					{ Atom $1 }

list :: Cons
	= cons ' '* list		{ Cons $1 $3 }
	/ '.' ' '* cons			{ $2 }
	/ ''				{ Atom Null }

atom :: Atom
	= 'define'			{ Define }
	/ 'lambda'			{ Lambda }
	/ 'cond'			{ Cond }
	/ '#f'				{ F }
	/ '#t'				{ T }
	/ 'nil'				{ Null }
	/ variable			{ Variable $1 }
	/ int '.' int			{ Double $ read $ $1 ++ "." ++ $2 }
	/ int				{ Int $ read $1 }
	/ '()'				{ Null }

int :: String
	= [0-9]+

variable :: String
	= [-+*/_<=>a-z]+		{ $1 }

|]
