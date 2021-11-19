module Main where

import "base" Control.Monad
import "base" Control.Exception
import "base" Data.Traversable (for)
import "base" System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import "sqlite-simple" Database.SQLite.Simple (Connection, Query, open, query_, execute)
import "transformers" Data.Functor.Reverse (Reverse (Reverse))
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)

import Control.SQL.Query (start_of_today, end_of_today, today_events_query)
import Control.Interface.TUI (Zipper (Zipper), focus, up, down)

keystroke :: Connection -> Char -> StateT (Zipper (Int, Int, String, String, String)) IO ()
keystroke _ 'j' = modify down *> lift (putStr "\ESC[1B")
keystroke _ 'k' = modify up *> lift (putStr "\ESC[1A")
keystroke connection 'r' = load_tasks connection
keystroke connection 'D' = do
	focus <$> get >>= \(task_id, status, title, start, stop) -> do
		lift $ execute connection update_task_status (0 :: Int, task_id)
		modify $ \(Zipper bs _ fs) -> Zipper bs (task_id, 0, title, start, stop) fs
keystroke connection 'C' = do
	focus <$> get >>= \(task_id, status, title, start, stop) -> do
		lift $ execute connection update_task_status (-1 :: Int, task_id)
		modify $ \(Zipper bs _ fs) -> Zipper bs (task_id, -1, title, start, stop) fs
keystroke connection 'T' = do
	focus <$> get >>= \(task_id, status, title, start, stop) -> do
		lift $ execute connection update_task_status (1 :: Int, task_id)
		modify $ \(Zipper bs _ fs) -> Zipper bs (task_id, 1, title, start, stop) fs
keystroke _ _ = pure ()

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

print_task :: (Int, Int, String, String, String) -> IO ()
print_task (_, -2, title, start, stop) = do
	putStrLn $ "[\ESC[0mLATE\ESC[0m] " <> start <> " - " <> stop <> " " <> title
print_task (_, -1, title, start, stop) = do
	putStrLn $ "[\ESC[0mGONE\ESC[0m] " <> start <> " - " <> stop <> " " <> title
print_task (_, 0, title, start, stop) = do
	putStrLn $ "[\ESC[4mDONE\ESC[0m] " <> start <> " - " <> stop <> " " <> title
print_task (_, 1, title, start, stop) = do
	putStrLn $ "[\ESC[1mTODO\ESC[0m] " <> start <> " - " <> stop <> " " <> title

today_tasks_query :: Query
today_tasks_query =
	"SELECT tasks.id, status, title, strftime('%H:%M', start, 'unixepoch', 'localtime'), strftime('%H:%M', stop, 'unixepoch', 'localtime') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start >= " <> start_of_today <> " AND stop <= " <> end_of_today <> " \
	\ORDER BY status;"

overdue_tasks_query :: Query
overdue_tasks_query =
	"SELECT tasks.id, -2, title, strftime('%H:%M', start, 'unixepoch', 'localtime'), strftime('%H:%M', stop, 'unixepoch', 'localtime') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE status = 1 and stop < strftime('%s', 'now') \
	\ORDER BY start;"

load_tasks connection = do
	today <- lift $ query_ @(Int, Int, String, String, String) connection today_tasks_query
	overdue <- lift $ query_ @(Int, Int, String, String, String) connection overdue_tasks_query
	put $ Zipper overdue (head today) (tail today)

main = do
	connection <- open "facts.db"
	today <- query_ @(Int, Int, String, String, String) connection today_tasks_query
	overdue <- query_ @(Int, Int, String, String, String) connection overdue_tasks_query
	hSetBuffering stdin NoBuffering
	hSetEcho stdin False
	putStr "\ESC[?25l"
	flip evalStateT (Zipper overdue (head today) (tail today))
		$ forever $ display *> (lift getChar >>= keystroke connection)
