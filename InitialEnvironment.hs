{-# LANGUAGE PackageImports #-}

module InitialEnvironment (
	initialEnvironment,
	liftIO,
	runStateT,
	parse,
	multiParse,
	runErrorT,
	eval,
	RunError(..),
	Run,
	Object
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
	("cond", Syntax cond),
	("if", Syntax if'),
	("and", Syntax and'),
	("or", Syntax or'),
	("not", Subroutine not'),
	("begin", Subroutine begin),
	("print", Subroutine print')
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
sub [Double n] = return $ Double $ - n
sub (Int n : ns)
	| any isDouble ns = return $ Double $ (fromIntegral n -) $ sum $
		map getDouble ns
	| otherwise = return $ Int $ (n -) $ sum $ map getInt ns
sub args = fail $ "subr -: " ++ show args

mul ns	| any isDouble ns = return $ Double $ product $ map getDouble ns
	| otherwise = return $ Int $ product $ map getInt ns

div' [] = fail "this procedure required at least one argument"
div' [Int n] = return $ Double $ 1 / fromIntegral n
div' (Int n : ns) = return $ Double $ (fromIntegral n /) $ fromIntegral $
	product $ map getInt ns
div' (Double d : ds) = return $ Double $ (d /) $ fromIntegral $
	product $ map getInt ds
div' args = fail $ "subr /: " ++ show args

isLarger, equal, isSmaller :: [Object] -> Run Object
isLarger [Int n1, Int n2]
	| n1 > n2 = return T
	| otherwise = return F
isLarger [n1, n2]
	| isDouble n1 || isDouble n2 = return $
		if getDouble n1 > getDouble n2 then T else F
isLarger args = fail $ "subr >: " ++ show args
equal [Int n1, Int n2]
	| n1 == n2 = return T
	| otherwise = return F
equal [n1, n2]
	| isDouble n1 || isDouble n2 = return $
		if getDouble n1 == getDouble n2 then T else F
equal args = fail $ "subr =: " ++ show args
isSmaller [Int n1, Int n2]
	| n1 < n2 = return T
	| otherwise = return F
isSmaller [n1, n2]
	| isNum n1 && isNum n2 && (isDouble n1 || isDouble n2) = return $
		if getDouble n1 < getDouble n2 then T else F
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
cond (Cons (Cons (Variable "else") body) _) = begin' body
cond (Cons (Cons p body) t) = do
	b <- eval p
	case b of
		F -> cond t
		_ -> begin' body
cond obj = fail $ "syntax cond: " ++ show obj

begin' :: Object -> Run Object
begin' (Cons v Null) = eval v
begin' (Cons v vs) = eval v >> begin' vs
begin' obj = fail $ "syntax begin': " ++ show obj

begin :: [Object] -> Run Object
begin args = return $ last args

if' :: Object -> Run Object
if' (Cons p (Cons t (Cons e _))) = do
	b <- eval p
	eval $ case b of
		F -> e
		_ -> t
if' obj = fail $ "syntax if: " ++ show obj

and', or' :: Object -> Run Object

and' Null = return T
and' (Cons p rest) = do
	b <- eval p
	case b of
		F -> return F
		_ -> and' rest
and' obj = fail $ "syntax and: " ++ show obj

or' Null = return F
or' (Cons p rest) = do
	b <- eval p
	case b of
		F -> or' rest
		_ -> return T
or' obj = fail $ "syntax or: " ++ show obj

not' :: [Object] -> Run Object
not' [F] = return T
not' [_] = return F
not' args = fail $ "subr not: " ++ show args

print' :: [Object] -> Run Object
print' [arg] = liftIO $ print arg >> return Undef
print' args = fail $ "subr print: " ++ show args
