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
eval (Cons (Atom Define) (Cons (Atom (Variable var)) (Cons val _))) = do
	env <- get
	r <- eval val
	put $ (var, r) : env
	return $ Variable var
eval (Cons fun args) = do
	env <- get
	Function f <- eval fun
	as <- mapCons eval args
	f as
eval (Atom (Variable var)) = do
	env <- get
	case lookup var env of
		Just val -> return val
		_ -> fail $ "no such var: " ++ var
eval (Atom p) = return p

mapCons :: Applicative m => (Cons -> m a) -> Cons -> m [a]
mapCons _ (Atom Null) = pure []
mapCons f (Cons a d) = (:) <$> f a <*> mapCons f d
