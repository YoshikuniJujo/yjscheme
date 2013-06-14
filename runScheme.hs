module Main where

import System.IO
import Control.Applicative
import Control.Monad
import InitialEnvironment
import System.Exit

main :: IO ()
main = do
	(r, _) <- flip runStateT initialEnvironment $ doWhileR $ do
		liftIO $ putStr "> " >> hFlush stdout
		parsed <- liftIO $ parse <$> getLine
		case parsed of
			Just cons -> do
				s <- runErrorT $ eval cons
				case s of
					Right r -> do
						liftIO $ print r
						return Nothing
					Left (Fail msg) -> do
						liftIO $ putStrLn $ "error: " ++ msg
						return Nothing
					Left (Exit n) -> return $ Just n
			_ -> do	liftIO $ putStrLn "parse error"
				return Nothing
	exitWith $ ExitFailure r

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ action = action >>= flip when (doWhile_ action)

doWhileR :: Monad m => m (Maybe a) -> m a
doWhileR action = do
	r <- action
	case r of
		Nothing -> doWhileR action
		Just n -> return n
