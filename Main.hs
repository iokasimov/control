module Main where

import "base" Control.Monad (forever, void)
import "base" System.IO (hFlush, stdout)
import "haskeline" System.Console.Haskeline (runInputT, defaultSettings)
import "sqlite-simple" Database.SQLite.Simple (open, query_)
import "text" Data.Text (Text)
import "pretty-terminal" System.Console.Pretty (Color (..), Style (..), style, bgColor, color)
import "transformers" Control.Monad.Trans.State (evalStateT)
import "vty" Graphics.Vty (standardIOConfig, mkVty)

import Control.Objective (Objective)
import Control.Interface.REPL (loop)
import Control.Interface.TUI (Zipper (Zipper), handler)
import Control.SQL.Query (all_unfinished_events, today_tasks_query, today_events_query)

print_timeline :: [(String, String, String, String)] -> IO ()
print_timeline = void . traverse (putStrLn . prepare) where

	prepare (title, start, stop, amount) =
		" ├─ " <> start <> "-" <> stop <> " (" <> amount <> ") " <> title

print_task :: (Int, String, Maybe String, Maybe String) -> IO ()
print_task (-1, title, start, stop) = putStrLn $ " [CANCELED] ("
	<> maybe "..:.." id start <> " - " <> maybe "..:.." id stop <> ") " <> title <> " "
print_task (0, title, start, stop) = putStrLn $ " [DONE] ("
	<> maybe "..:.." id start <> " - " <> maybe "..:.." id stop <> ") " <> title <> " "
print_task (1, title, start, stop) = putStrLn $ " [" <> style Bold "TODO" <> "] ("
	<> maybe "..:.." id start <> " - " <> maybe "..:.." id stop <> ") " <> title <> " "

--done_label = bgColor Blue . color White
--query_ @Objective connection "SELECT id, title FROM objectives" >>= \case
--	o : os -> evalStateT (handler vty) $ ("", Zipper [] o os)
--vty <- mkVty =<< standardIOConfig
--putStrLn $ bgColor White . color Black $ "TASKS"
--query_ @(Int, String, Maybe String, Maybe String) connection today_tasks_query >>= void . traverse print_task
--putStrLn $ bgColor White . color Black $ "EVENTS"
--query_ @(String, String, String, String) connection today_events_query >>= print_timeline

main = do
	connection <- open "facts.db"
	unfinished <- query_ connection all_unfinished_events
	void $ evalStateT (runInputT defaultSettings loop) (unfinished, Nothing)
