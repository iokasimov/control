module Main where

import "base" Control.Monad (void)
import "haskeline" System.Console.Haskeline (runInputT, defaultSettings)
import "sqlite-simple" Database.SQLite.Simple (open, query_)
import "transformers" Control.Monad.Trans.State (evalStateT)

import Control.Interface.REPL (loop)
import Control.SQL.Query (all_unfinished_events)

main = do
	connection <- open "facts.db"
	unfinished <- query_ connection all_unfinished_events
	void $ evalStateT (runInputT defaultSettings loop) (unfinished, Nothing)
