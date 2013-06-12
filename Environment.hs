module Environment (
	Environment,
	initialEnvironment,

	module Cons
) where

import Cons

type Environment = [(String, Primitive)]

initialEnvironment = [
	("+", Function add)
 ]

add :: [Primitive] -> Primitive
add = Int . sum . map getInt
