{-# LANGUAGE QuasiQuotes, FlexibleContexts, PackageImports #-}
{-# OPTIONS_GHC
	-fno-warn-unused-do-bind
	-fno-warn-unused-binds
	-fno-warn-name-shadowing
	-fno-warn-unused-matches #-}

module Object (
	Object(..),
	isNum,
	isDouble,
	getDouble,
	multiParse,
	parse,
	Environment,
	Run,
	RunError(..)
) where

import Text.Peggy (peggy, space, defaultDelimiter, parseString)
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.State
import "transformers" Control.Monad.Trans.Error
import Data.Maybe

type Environment = [(String, Object)]
type Run = ErrorT RunError (StateT Environment IO)

data RunError
	= Exit
	| ExitWith Int
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
	show Clojure{} = "#<clojure _>"

showCons :: Object -> String
showCons (Cons a d@(Cons _ _)) = show a ++ " " ++ showCons d
showCons (Cons a Null) = show a
showCons (Cons a d) = show a ++ " . " ++ show d
showCons obj = error $ "showCons: " ++ show obj

instance Read Object where
	readsPrec _ inp = case parse inp of
		Just r -> [(r, "")]
		Nothing -> []

isNum :: Object -> Bool
isNum (Int _) = True
isNum (Double _) = True
isNum _ = False

isDouble :: Object -> Bool
isDouble (Double _) = True
isDouble _ = False

getDouble :: Object -> Double
getDouble (Int n) = fromIntegral n
getDouble (Double d) = d
getDouble _ = error "getDouble: not number"

multiParse :: String -> Maybe [Object]
multiParse = either (const Nothing) Just . parseString multiCons ""

parse :: String -> Maybe Object
parse = either (const Nothing) Just . parseString cons ""

[peggy|

multiCons :: [Object]
	= sp (cons sp)*				{ map fst $2 }

cons :: Object
	= '(' sp cons sp '.' sp cons ')'	{ Cons $2 $5 }
	/ '(' list ')'				{ $1 }
	/ atom					{ $1 }

list :: Object
	= cons sp list		{ Cons $1 $3 }
	/ '.' sp cons			{ $2 }
	/ ''				{ Null }

atom :: Object
	= '#f'				{ F }
	/ '#t'				{ T }
	/ 'nil'				{ Null }
	/ sinedInt '.' int		{ Double $ read $ $1 ++ "." ++ $2 }
	/ sinedInt			{ Int $ read $1 }
	/ variable			{ Variable $1 }
	/ '()'				{ Null }

sinedInt :: String
	= '-'? int			{ fromMaybe "" $1 ++ $2 }

int :: String
	= [0-9]+

variable :: String
	= [-+*/_<=>a-z]+		{ $1 }

sp :: ()
	= (' ' / '\t' / '\n')*		{ () }

|]
