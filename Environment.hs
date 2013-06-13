{-# LANGUAGE PackageImports #-}

module Environment (
	initialEnvironment,

	module Cons
) where

import Cons
import System.Exit
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.State

initialEnvironment = [
	("exit", Function exit),
	("+", Function add),
	("-", Function sub),
	("*", Function mul),
	("/", Function div')
 ]

exit :: [Primitive] -> ErrorT String (StateT Environment IO) Primitive
exit [] = liftIO exitSuccess
exit [Int n] = liftIO $ exitWith $ ExitFailure n
exit _ = fail "Usage: (exit [exit status])"

add, sub, mul, div' :: [Primitive] -> ErrorT String (StateT Environment IO) Primitive

add ns	| any isDouble ns = return $ Double $ sum $ map getDouble ns
	| otherwise = return $ Int $ sum $ map getInt ns

sub [] = fail "this procedure required at least one argument"
sub [Int n] = return $ Int $ - n
sub (Int n : ns) = return $ Int $ (n -) $ sum $ map getInt ns

mul ns	| any isDouble ns = return $ Double $ product $ map getDouble ns
	| otherwise = return $ Int $ product $ map getInt ns

div' [] = fail "this procedure required at least one argument"
div' [Int n] = return $ Double $ 1 / fromIntegral n
div' (Int n : ns) = return $ Double $ (fromIntegral n /) $ fromIntegral $
	product $ map getInt ns

{-
define :: StateT Environment (ErrorT String IO) Primitive
define [Variable
define _ = fail "bad arguments"
-}
