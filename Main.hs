module Main where

import "base" Control.Monad (forever, void)
import "base" System.IO (hFlush, stdout)
import "haskeline" System.Console.Haskeline (runInputT, defaultSettings)
import "sqlite-simple" Database.SQLite.Simple (open, query_)
import "text" Data.Text (Text)
import "transformers" Control.Monad.Trans.State (evalStateT)
import "vty" Graphics.Vty (standardIOConfig, mkVty)

import Control.Objective (Objective)
import Control.Interface.REPL (loop, print_task, print_timeline)
import Control.Interface.TUI (Zipper (Zipper), handler)
import Control.SQL.Query (all_unfinished_events, today_tasks_query, today_events_query)


main = do
	connection <- open "facts.db"
	--unfinished <- query_ connection all_unfinished_events
	vty <- mkVty =<< standardIOConfig
	putStrLn "\n"
	putStrLn " AGENDA FOR TODAY: "
	putStrLn "\n"
	query_ @(Int, String, Maybe String, Maybe String) connection today_tasks_query >>= void . traverse print_task
	putStrLn "\n"
	query_ @(String, String, String, String) connection today_events_query >>= print_timeline
	putStrLn "\n"

	--evalStateT (prompt') (unfinished, Nothing)
	--void $ evalStateT (runInputT defaultSettings loop) (unfinished, Nothing)
	--query_ @Objective connection "SELECT id, title FROM objectives" >>= \case
	--	o : os -> evalStateT (handler vty) $ ("", Zipper [] o os)
