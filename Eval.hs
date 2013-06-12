module Eval (
	eval,

	parse,
	Cons(..),
	Atom(..),
) where

import Environment

import Control.Applicative

eval :: Environment -> Cons -> Either String Primitive
eval _ (Atom (Primitive p)) = return p
eval env (Cons fun args) = do
	Function f <- eval env fun
	as <- mapCons (eval env) args
	return $ f as
eval env (Atom (Variable var)) = case lookup var env of
	Just val -> return val
	_ -> Left $ "no such var: " ++ var

mapCons :: Applicative m => (Cons -> m a) -> Cons -> m [a]
mapCons _ (Atom Null) = pure []
mapCons f (Cons a d) = (:) <$> f a <*> mapCons f d
