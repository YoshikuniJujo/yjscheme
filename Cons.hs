{-# LANGUAGE QuasiQuotes, FlexibleContexts, PackageImports #-}

module Cons (
	Cons(..),
	isDouble,
	getDouble,
	Atom(..),
	Primitive(..),
	parse
) where

import Text.Peggy (peggy, space, defaultDelimiter, parseString)
import "monads-tf" Control.Monad.Error

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

data Atom = Primitive Primitive | Variable String | Null

instance Show Atom where
	show (Primitive p) = show p
	show (Variable v) = v
	show Null = "nil"

data Primitive
	= Int { getInt :: Int }
	| Double Double
	| Function ([Primitive] -> ErrorT String IO Primitive)

isDouble (Double _) = True
isDouble _ = False

getDouble :: Primitive -> Double
getDouble (Int n) = fromIntegral n
getDouble (Double d) = d

instance Show Primitive where
	show (Int n) = show n
	show (Double d) = show d
	show (Function _) = "(Function _)"

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
	= primitive			{ Primitive $1 }
	/ variable			{ Variable $1 }
	/ 'nil'				{ Null }

primitive :: Primitive
	= int '.' int			{ Double $ read $ $1 ++ "." ++ $2 }
	/ int				{ Int $ read $1 }

int :: String
	= [0-9]+

variable :: String
	= [-+*/_a-z]+			{ $1 }

|]