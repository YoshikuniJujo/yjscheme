module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Eval

main :: IO ()
main = doWhile_ $ do
	putStr "> " >> hFlush stdout
	parsed <- parse <$> getLine
	case parsed of
		Just cons -> do
			s <- runErrorT $ eval initialEnvironment cons
			case s of
				Right r -> print r
				Left err -> putStrLn $ "error: " ++ err
			return True
		_ -> do	putStrLn "parse error"
			return True

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ action = action >>= flip when (doWhile_ action)
