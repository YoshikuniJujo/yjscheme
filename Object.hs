{-# LANGUAGE QuasiQuotes, FlexibleContexts, PackageImports #-}

module Object (
	Object(..),
	isDouble,
	getDouble,
	parse,
	Environment
) where

import Text.Peggy (peggy, space, defaultDelimiter, parseString)
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.State

type Environment = [(String, Object)]

data Object
	= Cons { car :: Object, cdr :: Object }
	| Undef
	| Null
	| F
	| T
	| Define
	| Lambda
	| Cond
	| Variable String
	| Int { getInt :: Int }
	| Double Double
	| Function ([Object] -> ErrorT String (StateT Environment IO) Object)
	| Clojure Environment [String] Object

instance Show Object where
	show c@(Cons _ _) = "(" ++ showCons c ++ ")"
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

showCons :: Object -> String
showCons (Cons a d@(Cons _ _)) = show a ++ " " ++ showCons d
showCons (Cons a Null) = show a
showCons (Cons a d) = show a ++ " . " ++ show d

instance Read Object where
	readsPrec 0 inp = case parse inp of
		Just r -> [(r, "")]
		Nothing -> []

isDouble (Double _) = True
isDouble _ = False

getDouble :: Object -> Double
getDouble (Int n) = fromIntegral n
getDouble (Double d) = d

parse :: String -> Maybe Object
parse = either (const Nothing) Just . parseString cons ""

[peggy|

cons :: Object
	= '(' cons ' '* '.' ' '* cons ')'	{ Cons $1 $4 }
	/ '(' list ')'				{ $1 }
	/ atom					{ $1 }

list :: Object
	= cons ' '* list		{ Cons $1 $3 }
	/ '.' ' '* cons			{ $2 }
	/ ''				{ Null }

atom :: Object
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
