{-# LANGUAGE PackageImports #-}

module InitialEnvironment (
	initialEnvironment,
	liftIO,
	runStateT,
	parse,
	runErrorT,
	eval,
	RunError(..)
) where

import Eval
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.State

initialEnvironment :: Environment
initialEnvironment = [
	("exit", Subroutine exit),
	("+", Subroutine add),
	("-", Subroutine sub),
	("*", Subroutine mul),
	("/", Subroutine div'),
	(">", Subroutine isLarger),
	("=", Subroutine equal),
	("<", Subroutine isSmaller),
	("define", Syntax define),
	("lambda", Syntax lambda),
	("cond", Syntax cond)
 ]

exit :: [Object] -> Run Object
exit [] = throwError Exit
exit [Int n] = throwError $ ExitWith n
exit _ = fail "Usage: (exit [exit status])"

add, sub, mul, div' :: [Object] -> Run Object

add ns	| any isDouble ns = return $ Double $ sum $ map getDouble ns
	| otherwise = return $ Int $ sum $ map getInt ns

sub [] = fail "this procedure required at least one argument"
sub [Int n] = return $ Int $ - n
sub (Int n : ns) = return $ Int $ (n -) $ sum $ map getInt ns
sub args = fail $ "subr -: " ++ show args

mul ns	| any isDouble ns = return $ Double $ product $ map getDouble ns
	| otherwise = return $ Int $ product $ map getInt ns

div' [] = fail "this procedure required at least one argument"
div' [Int n] = return $ Double $ 1 / fromIntegral n
div' (Int n : ns) = return $ Double $ (fromIntegral n /) $ fromIntegral $
	product $ map getInt ns
div' args = fail $ "subr /: " ++ show args

isLarger, equal, isSmaller :: [Object] -> Run Object
isLarger [Int n1, Int n2]
	| n1 > n2 = return T
	| otherwise = return F
isLarger _ = fail "bad argument"
equal [Int n1, Int n2]
	| n1 == n2 = return T
	| otherwise = return F
equal args = fail $ "subr =: " ++ show args
isSmaller [Int n1, Int n2]
	| n1 < n2 = return T
	| otherwise = return F
isSmaller args = fail $ "subr <: " ++ show args

define, lambda :: Object -> Run Object

define (Cons v@(Variable var) (Cons val _)) = do
	r <- eval val
	modify ((var, r) :)
	return v
define (Cons (Cons fn@(Variable _) vars) body) = do
	cl <- lambda $ Cons vars body
	_ <- define $ Cons fn $ Cons cl undefined
	return fn
define c = fail $ "syntax define: " ++ show c

lambda (Cons cvars (Cons body _)) = do
	env <- get
	vars <- mapCons (\(Variable var) -> return var) cvars
	return $ Clojure env vars body
lambda obj = fail $ "syntax lambda: " ++ show obj

cond :: Object -> Run Object
cond Null = return Undef
cond (Cons (Cons p (Cons body _)) t) = do
	b <- eval p
	case b of
		F -> cond t
		_ -> eval body
cond obj = fail $ "syntax cond: " ++ show obj
