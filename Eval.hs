{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}

module Eval (
	eval,
	runErrorT,

	parse,
	initialEnvironment
) where

import Environment

import Control.Applicative
import "monads-tf" Control.Monad.Error

eval :: Environment -> Cons -> ErrorT String IO Primitive
eval _ (Atom (Primitive p)) = lift $ return p
eval env (Cons fun args) = do
	Function f <- eval env fun
	as <- mapCons (eval env) args
	f as
eval env (Atom (Variable var)) = case lookup var env of
	Just val -> return val
	_ -> fail $ "no such var: " ++ var

mapCons :: Applicative m => (Cons -> m a) -> Cons -> m [a]
mapCons _ (Atom Null) = pure []
mapCons f (Cons a d) = (:) <$> f a <*> mapCons f d
