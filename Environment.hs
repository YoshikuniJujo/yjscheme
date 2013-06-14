{-# LANGUAGE PackageImports #-}

module Environment (
	initialEnvironment,

	module Object
) where

import Object
import System.Exit
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.State

initialEnvironment = [
	("exit", Function exit),
	("+", Function add),
	("-", Function sub),
	("*", Function mul),
	("/", Function div'),
	(">", Function isLarger),
	("=", Function equal),
	("<", Function isSmaller)
 ]

exit :: [Object] -> ErrorT String (StateT Environment IO) Object
exit [] = liftIO exitSuccess
exit [Int n] = liftIO $ exitWith $ ExitFailure n
exit _ = fail "Usage: (exit [exit status])"

add, sub, mul, div' :: [Object] -> ErrorT String (StateT Environment IO) Object

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

isLarger, equal, isSmaller :: [Object] -> ErrorT String (StateT Environment IO) Object
isLarger [Int n1, Int n2]
	| n1 > n2 = return T
	| otherwise = return F
isLarger _ = fail "bad argument"
equal [Int n1, Int n2]
	| n1 == n2 = return T
	| otherwise = return F
isSmaller [Int n1, Int n2]
	| n1 < n2 = return T
	| otherwise = return F
