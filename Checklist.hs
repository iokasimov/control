module Main where

import "base" Control.Monad
import "base" Control.Exception
import "base" Data.Functor ((<&>))
import "base" Data.Traversable (for)
import "base" Data.Maybe (catMaybes)
import "base" System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import "joint" Control.Joint ((<$$>), (<**>))
import "sqlite-simple" Database.SQLite.Simple (Connection, Query, open, query_, execute)
import "transformers" Data.Functor.Reverse (Reverse (Reverse))
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)

import Control.SQL.Query (start_of_today, end_of_today, start_of_tomorrow, end_of_tomorrow)
import Control.Interface.TUI (Zipper (Zipper), focus, up, down)

type Task = (Int, Int, Int, String, String, String)

keystroke :: Connection -> Char -> StateT (Zipper (String, Zipper Task)) IO ()
keystroke _ '\t' = modify $ \case
	Zipper [] x [] -> Zipper [] x []
	Zipper bs x [] -> Zipper [] (last bs) $ reverse (init bs) <> [x]
	Zipper bs x (f : fs) -> Zipper (x : bs) f fs
keystroke _ 'j' = do
	modify $ \(Zipper bs (title, x) fs) -> Zipper bs (title, down x) fs
keystroke _ 'k' = do
	modify $ \(Zipper bs (title, x) fs) -> Zipper bs (title, up x) fs
keystroke connection 'r' = lift (refresh_tasks connection) >>= put
keystroke connection 'D' = do
	focus . snd . focus <$> get >>= \(task_id, status, mode, title, start, stop) -> do
		lift $ execute connection update_task_status (0 :: Int, task_id)
		modify $ \(Zipper bs (title, Zipper bbs x ffs) fs) ->
			Zipper bs (title, Zipper bbs (task_id, 0, mode, title, start, stop) ffs) fs
keystroke connection 'C' = do
	focus . snd . focus <$> get >>= \(task_id, status, mode, title, start, stop) -> do
		lift $ execute connection update_task_status (-1 :: Int, task_id)
		modify $ \(Zipper bs (title, Zipper bbs x ffs) fs) ->
			Zipper bs (title, Zipper bbs (task_id, -1, mode, title, start, stop) ffs) fs
keystroke connection 'T' = do
	focus . snd . focus <$> get >>= \(task_id, status, mode, title, start, stop) -> do
		lift $ execute connection update_task_status (1 :: Int, task_id)
		modify $ \(Zipper bs (title, Zipper bbs x ffs) fs) ->
			Zipper bs (title, Zipper bbs (task_id, 1, mode, title, start, stop) ffs) fs
keystroke _ _ = pure ()

update_task_status :: Query
update_task_status = "UPDATE tasks SET status = ? WHERE id = ?"

display = do
	lift $ putStr "\ESC[2J"
	lift $ putStr "\ESC[100A"
	get >>= \(Zipper bs x fs) -> lift $ do
		traverse print_unfocused_tasks $ Reverse bs
		putStr "\n"
		print_focused_tasks  x
		traverse print_unfocused_tasks fs
		putStr "\n"

print_focused_tasks (title, Zipper bs x fs) = void $ do
	putStrLn $ " + \ESC[1m\ESC[4m" <> title <> "\ESC[0m"
	print_tasks $ Reverse bs
	print_focused_task x
	print_tasks $ fs

print_unfocused_tasks (title, Zipper bs x fs) = void $ do
	putStrLn $ "\n   \ESC[4m" <> title <> "\ESC[0m"
	print_tasks $ Reverse bs
	print_unfocused_task x
	print_tasks $ fs

print_tasks tasks = for tasks $ \t ->
	putStrLn $ "   " <> show_task t

print_focused_task t =
	putStrLn $ " * \ESC[1m" <> show_task t <> "\ESC[0m"

print_unfocused_task t =
	putStrLn $ "   " <> show_task t

show_task :: Task -> String
show_task (_, status, mode, title, start, stop) = 
	show_task_status status <> show_task_boundaries mode start stop <> title

show_task_status (-2) = "[LATE] "
show_task_status (-1) = "[GONE] "
show_task_status 0 = "[DONE] "
show_task_status 1 = "[TODO] "

show_task_boundaries 0 ready [] = "[READY: " <> ready <> "] "
show_task_boundaries 0 ready deadline = "[READY: " <> ready <> "] [DEADLINE: " <> deadline <> "] "
show_task_boundaries 1 begin [] = "[BEGIN: " <> begin <> "] "
show_task_boundaries 1 begin complete = "[BEGIN: " <> begin <> "] [COMPLETE: " <> complete <> "] "

title_with_counter title count =
	title <> " (" <> show count <> ")"

today_tasks :: Query
today_tasks =
	"SELECT tasks.id, status, task_event_priority, title, \
	\strftime('%H:%M', start, 'unixepoch', 'localtime'), \
	\IFNULL(strftime('%H:%M', stop, 'unixepoch', 'localtime'), '') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start >= " <> start_of_today <> " AND IFNULL(stop <= " <> end_of_today <> ", 1) \
	\ORDER BY status, task_event_priority, start;"

tomorrow_tasks :: Query
tomorrow_tasks =
	"SELECT tasks.id, status, task_event_priority, title, \
	\strftime('%H:%M', start, 'unixepoch', 'localtime'), \
	\IFNULL(strftime('%H:%M', stop, 'unixepoch', 'localtime'), '') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start >= " <> start_of_tomorrow <> " AND IFNULL(stop <= " <> end_of_tomorrow <> ", 1) \
	\ORDER BY status, task_event_priority, start;"

someday_todo :: Query
someday_todo =
	"SELECT tasks.id, 1, task_event_priority, title, strftime('%d.%m', start, 'unixepoch', 'localtime'), '' \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE stop IS NULL AND status = 1 AND task_event_priority = 0;"

overdue :: Query
overdue =
	"SELECT tasks.id, -2, task_event_priority, title, strftime('%d.%m %H:%M', start, 'unixepoch', 'localtime'), strftime('%d.%m %H:%M', stop, 'unixepoch', 'localtime') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE status = 1 and stop < strftime('%s', 'now') \
	\ORDER BY start;"

load_all_tasks connection = (\od td tm sd -> od : td : tm : sd : [])
	<$> load_tasks_zipper connection "OVERDUE tasks" overdue
	<*> load_tasks_zipper connection "Tasks for TODAY" today_tasks
	<*> load_tasks_zipper connection "Tasks for TOMORROW" tomorrow_tasks
	<*> load_tasks_zipper connection "Tasks to do SOMEDAY" someday_todo

load_tasks_zipper :: Connection -> String -> Query -> IO (Maybe (String, Zipper Task))
load_tasks_zipper connection title q = query_ @Task connection q <&> \case
	t : ts -> Just $ (title, Zipper [] t ts)
	[] -> Nothing

refresh_tasks connection = do
	load_all_tasks connection <&> catMaybes <&> \case
		[] -> error "There are no tasks!"
		s : ss -> Zipper [] s ss

main = do
	connection <- open "facts.db"
	hSetBuffering stdin NoBuffering
	hSetEcho stdin False
	putStr "\ESC[?25l"

	--query_ @Task connection today_tasks >>= \case
	--	t : ts -> print_focused_tasks ("TODAY", Zipper [] t ts)

	refresh_tasks connection >>= evalStateT
		(forever $ display *> (lift getChar >>= keystroke connection))
	--flip evalStateT (Zipper (overdue <> still) (head today) (tail today))
