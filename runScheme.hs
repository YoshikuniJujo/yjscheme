module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Cons

main :: IO ()
main = doWhile_ $ do
	putStr "> " >> hFlush stdout
	parsed <- parse <$> getLine
	case parsed of
		Just (Cons (Atom (Variable "exit")) (Atom Null)) -> return False
		Just cons -> do
			print cons
			return True
		_ -> do	putStrLn "parse error"
			return True

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ action = action >>= flip when (doWhile_ action)
