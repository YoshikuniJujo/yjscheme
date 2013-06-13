module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Eval

main :: IO ()
main = (>> return ()) $ flip runStateT initialEnvironment $ doWhile_ $ do
	liftIO $ putStr "> " >> hFlush stdout
	parsed <- liftIO $ parse <$> getLine
	case parsed of
		Just cons -> do
			s <- runErrorT $ eval cons
			case s of
				Right r -> liftIO $ print r
				Left err -> liftIO $ putStrLn $ "error: " ++ err
			return True
		_ -> do	liftIO $ putStrLn "parse error"
			return True

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ action = action >>= flip when (doWhile_ action)
