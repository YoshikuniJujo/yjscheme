{-# LANGUAGE PackageImports #-}

module Environment (
	Environment,
	initialEnvironment,

	module Cons
) where

import Cons
import System.Exit
import "monads-tf" Control.Monad.Error

type Environment = [(String, Primitive)]

initialEnvironment = [
	("exit", Function exit),
	("+", Function add),
	("-", Function sub),
	("*", Function mul),
	("/", Function div')
 ]

exit :: [Primitive] -> ErrorT String IO Primitive
exit [] = lift exitSuccess
exit [Int n] = lift $ exitWith $ ExitFailure n
exit _ = fail "Usage: (exit [exit status])"

add, sub, mul, div' :: [Primitive] -> ErrorT String IO Primitive

add ns	| any isDouble ns = return $ Double $ sum $ map getDouble ns
	| otherwise = return $ Int $ sum $ map getInt ns

sub [] = fail "this procedure required at least one argument"
sub [Int n] = return $ Int $ - n
sub (Int n : ns) = return $ Int $ (n -) $ sum $ map getInt ns

mul = return . Int . product . map getInt

div' [] = fail "this procedure required at least one argument"
div' [Int n] = return $ Double $ 1 / fromIntegral n
div' (Int n : ns) = return $ Double $ (fromIntegral n /) $ fromIntegral $
	product $ map getInt ns
