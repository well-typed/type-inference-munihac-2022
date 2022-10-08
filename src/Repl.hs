module Repl where

import Control.Monad.State.Strict
import Data.Map as M
import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty)

import Syntax
import Types
import Parser

dummyTypeOf :: Env -> Expr -> Either String PType
dummyTypeOf _ _ = Left "There is nothing you can do until you implement type inference"

repl :: Env -> (Env -> Expr -> Either String PType) -> IO ()
repl gamma0 ti = evalStateT (runInputT defaultSettings loop) gamma0
  where
    loop :: InputT (StateT Env IO) ()
    loop = do
      inp <- getInputLine "> "
      case inp of
        Nothing -> return ()
        Just xs -> do
          lift $ handler xs
          loop

    handler :: String -> StateT Env IO ()
    handler xs = case parseToplevel xs of
      Left err -> liftIO $ putStrLn $ errorBundlePretty err
      Right (n, e) -> do
        gamma <- get
        case ti gamma e of
          Left err -> liftIO $ putStrLn err
          Right t  -> do
            liftIO $ putStrLn $ pretty t
            put $ M.insert n t gamma

