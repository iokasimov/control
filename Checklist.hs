module Main where

import "base" Control.Monad
import "base" Control.Exception
import "base" Data.Traversable (for)
import "base" System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import "sqlite-simple" Database.SQLite.Simple (Query,open, query_)
import "transformers" Data.Functor.Reverse (Reverse (Reverse))
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)

import Control.SQL.Query (today_tasks_query)
import Control.Interface.TUI (Zipper (Zipper), focus, up, down)

keystroke :: Connection -> Char -> StateT (Zipper (Int, String, String, String)) IO ()
keystroke _ 'j' = modify down *> lift (putStr "\ESC[1B")
keystroke _ 'k' = modify up *> lift (putStr "\ESC[1A")

update_task_status :: Query
update_task_status = "UPDATE tasks SET status = ? WHERE id = ?"

display = do
	lift $ putStr "\ESC[2J"
	lift $ putStr "\ESC[100A"
	get >>= lift . print_zipper_items

print_zipper_items (Zipper bs x fs) = void $ do
	print_tasks $ Reverse bs
	print_focused_task x
	print_tasks $ fs

print_tasks tasks = for tasks $ \t ->
	putStr "   " *> print_task t

print_focused_task t =
	putStr " * " *> print_task t

print_task :: (Int, String, String, String) -> IO ()
print_task (-1, title, start, stop) = do
	putStrLn $ "[\ESC[0mGONE\ESC[0m] " <> start <> " - " <> stop <> " " <> title
print_task (0, title, start, stop) = do
	putStrLn $ "[\ESC[4mDONE\ESC[0m] " <> start <> " - " <> stop <> " " <> title
print_task (1, title, start, stop) = do
	putStrLn $ "[\ESC[1mTODO\ESC[0m] " <> start <> " - " <> stop <> " " <> title

main = do
	connection <- open "facts.db"
	today <- query_ @(Int, String, String, String) connection today_tasks_query
	hSetBuffering stdin NoBuffering
	hSetEcho stdin False
	putStr "\ESC[?25l"
	flip evalStateT (Zipper [] (head today) (tail today))
		$ forever $ display *> (lift getChar >>= keystroke)
