{-# LANGUAGE QuasiQuotes, FlexibleContexts, PackageImports #-}

module Object (
	Object(..),
	isDouble,
	getDouble,
	parse,
	Environment,
	Run,
	RunError(..)
) where

import Text.Peggy (peggy, space, defaultDelimiter, parseString)
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.State
import "transformers" Control.Monad.Trans.Error

type Environment = [(String, Object)]
-- type RunError = String
type Run = ErrorT RunError (StateT Environment IO)

data RunError
	= Exit Int
	| Fail String
	deriving Show

instance Error RunError where
	strMsg = Fail

data Object
	= Undef
	| Null
	| F
	| T
	| Int { getInt :: Int }
	| Double Double
	| Variable String
	| Syntax (Object -> Run Object)
	| Subroutine ([Object] -> Run Object)
	| Clojure Environment [String] Object
	| Cons { car :: Object, cdr :: Object }

instance Show Object where
	show c@(Cons _ _) = "(" ++ showCons c ++ ")"
	show Undef = "#<undef>"
	show Null = "()"
	show T = "#t"
	show F = "#f"
	show (Int n) = show n
	show (Double d) = show d
	show (Variable v) = v
	show (Syntax _) = "#<syntax _>"
	show (Subroutine _) = "#<subr _>"
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
	= '#f'				{ F }
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
