module Main where

import System.Environment
import System.IO
import Control.Applicative
import Control.Monad
import InitialEnvironment
import System.Exit

runFile :: FilePath -> Run Object
runFile fp = do
	cnt <- liftIO $ readFile fp
	case multiParse cnt of
		Just cons -> last <$> mapM eval cons
		_ -> fail $ "file " ++ fp ++ ": parse error"

main :: IO ()
main = do
	fns <- getArgs
	(r, _) <- flip runStateT initialEnvironment $ do
		ret <- runErrorT $ mapM_ runFile fns
		case ret of
			Left (Fail msg) -> liftIO $ putStrLn $ "error: " ++ msg
			_ -> return ()
		doWhileR $ do
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
						Left (ExitWith n) -> return $ Just $ ExitFailure n
						Left Exit -> return $ Just ExitSuccess
				_ -> do	liftIO $ putStrLn "parse error"
					return Nothing
	exitWith $ case r of
		ExitFailure 0 -> ExitSuccess
		_ -> r

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ action = action >>= flip when (doWhile_ action)

doWhileR :: Monad m => m (Maybe a) -> m a
doWhileR action = do
	r <- action
	case r of
		Nothing -> doWhileR action
		Just n -> return n
