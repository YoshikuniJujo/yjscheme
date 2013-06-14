{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}

module Eval (
	eval,
	runErrorT,
	runStateT,

	liftIO,
	mapCons,
	module Object
) where

import Object

import Control.Applicative
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.State

eval :: Object -> ErrorT String (StateT Environment IO) Object
eval (Cons (Clojure env vars body) cvars) = do
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
		Syntax s -> do
			s args
		Subroutine f -> do
			as <- mapCons eval args
			f as
		Clojure _ _ _ -> eval $ Cons fc args
		_ -> fail $ "bad: " ++ show fc
eval (Variable var) = do
	env <- get
	case lookup var env of
		Just val -> return val
		_ -> fail $ "no such var: " ++ var
eval p = return p

mapCons :: Applicative m => (Object -> m a) -> Object -> m [a]
mapCons _ Null = pure []
mapCons f (Cons a d) = (:) <$> f a <*> mapCons f d
