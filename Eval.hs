{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}

module Eval (
	eval,
	runErrorT,
	runStateT,

	parse,
	initialEnvironment,
	liftIO
) where

import Environment

import Control.Applicative
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.State

eval :: Cons -> ErrorT String (StateT Environment IO) Atom
eval (Cons (Atom Cond) sel) = cond sel
eval (Cons (Atom Define) (Cons (Atom (Variable var)) (Cons val _))) = do
	env <- get
	r <- eval val
	put $ (var, r) : env
	return $ Variable var
eval (Cons
	(Atom Define)
	(Cons (Cons (Atom (Variable fn)) vars) (Cons body _))) = do
	lambda <- eval $ Cons (Atom Lambda) $ Cons vars $ Cons body $ Atom Null
	eval $ Cons (Atom Define) $ Cons (Atom (Variable fn)) $
		Cons (Atom lambda) $ Atom Null
eval (Cons (Atom Lambda) (Cons cvars (Cons body _))) = do
	env <- get
	vars <- mapCons (\(Atom (Variable var)) -> return var) cvars
	return $ Clojure env vars body
eval (Cons (Atom (Clojure env vars body)) cvars) = do
	genv <- get
	vals <- mapCons eval cvars
	put $ zip vars vals ++ env
	ret <- eval body
	put genv
	return ret
eval (Cons fun args) = do
	env <- get
	fc <- eval fun
	case fc of
		Function f -> do
			as <- mapCons eval args
			f as
		Clojure _ _ _ -> eval $ Cons (Atom fc) args
		_ -> fail $ "bad: " ++ show fc
eval (Atom (Variable var)) = do
	env <- get
	case lookup var env of
		Just val -> return val
		_ -> fail $ "no such var: " ++ var
eval (Atom p) = return p

mapCons :: Applicative m => (Cons -> m a) -> Cons -> m [a]
mapCons _ (Atom Null) = pure []
mapCons f (Cons a d) = (:) <$> f a <*> mapCons f d

cond :: Cons -> ErrorT String (StateT Environment IO) Atom
cond (Atom Null) = return Undef
cond (Cons (Cons p (Cons body _)) t) = do
	b <- eval p
	case b of
		F -> cond t
		_ -> eval body
