module Main where

import "base" Control.Monad
import "base" Control.Exception
import "base" Data.Functor ((<&>))
import "base" Data.Traversable (for)
import "base" System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import "joint" Control.Joint ((<$$>), (<**>))
import "sqlite-simple" Database.SQLite.Simple (Connection, Query, open, query_, execute)
import "transformers" Data.Functor.Reverse (Reverse (Reverse))
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)

import Control.SQL.Query (start_of_today, end_of_today, today_events_query)
import Control.Interface.TUI (Zipper (Zipper), focus, up, down)

type Task = (Int, Int, Int, String, String, String)

keystroke :: Connection -> Char -> StateT (Zipper (String, Zipper Task)) IO ()
keystroke _ '\t' = modify $ \case
	Zipper [] x [] -> Zipper [] x []
	Zipper bs x [] -> Zipper [] (last bs) $ reverse (init bs) <> [x]
	Zipper bs x (f : fs) -> Zipper (x : bs) f fs
keystroke _ 'j' = do
	modify $ \(Zipper bs (title, x) fs) -> Zipper bs (title, down x) fs
	--lift (putStr "\ESC[1B")
keystroke _ 'k' = do
	modify $ \(Zipper bs (title, x) fs) -> Zipper bs (title, up x) fs
	--lift (putStr "\ESC[1A")
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
		traverse print_unfocused_tasks bs
		putStr "\n"
		print_focused_tasks  x
		traverse print_unfocused_tasks fs
		putStr "\n"

print_focused_tasks (title, Zipper bs x fs) = void $ do
	putStrLn $ "   \ESC[1m\ESC[4m" <> title <> "\ESC[0m"
	print_tasks $ Reverse bs
	print_focused_task x
	print_tasks $ fs

print_unfocused_tasks (title, Zipper bs x fs) = void $ do
	putStrLn $ "\n   \ESC[4m" <> title <> "\ESC[0m"
	print_tasks $ Reverse bs
	print_unfocused_task x
	print_tasks $ fs

print_tasks tasks = for tasks $ \t ->
	putStr "   " *> print_task t

print_focused_task t =
	putStr " > " *> print_task t

print_unfocused_task t =
	putStr "   " *> print_task t

print_task :: Task -> IO ()
print_task (_, status, mode, title, start, stop) = putStrLn
	$ show_task_status status <> show_task_mode mode <> start <> "-" <> stop <> "] " <> title

show_task_status (-2) = "[LATE] "
show_task_status (-1) = "[GONE] "
show_task_status 0 = "[DONE] "
show_task_status 1 = "[TODO] "

show_task_mode 0 = "[FLEXIBLE "
show_task_mode 1 = "[SCHEDULE "

today_todo :: Query
today_todo =
	"SELECT tasks.id, status, task_event_priority, title, strftime('%d.%m %H:%M', start, 'unixepoch', 'localtime'), IFNULL(strftime('%d.%m %H:%M', stop, 'unixepoch', 'localtime'), '.....') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start >= " <> start_of_today <> " AND IFNULL(stop <= " <> end_of_today <> ", 1) \
	\ORDER BY status;"

still_todo :: Query
still_todo =
	"SELECT tasks.id, 1, task_event_priority, title, strftime('%d.%m %H:%M', start, 'unixepoch', 'localtime'), IFNULL(strftime('%d.%m %H:%M', stop, 'unixepoch', 'localtime'), '.....') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start < " <> start_of_today <> " AND IFNULL(stop >= strftime('%s', 'now'), 1) AND status = 1;"

overdue :: Query
overdue =
	"SELECT tasks.id, -2, task_event_priority, title, strftime('%d.%m %H:%M', start, 'unixepoch', 'localtime'), strftime('%d.%m %H:%M', stop, 'unixepoch', 'localtime') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE status = 1 and stop < strftime('%s', 'now') \
	\ORDER BY start;"

load_all_tasks connection = (,,)
	<$> query_ @Task connection overdue
	<*> query_ @Task connection still_todo
	<*> query_ @Task connection todo_today

refresh_tasks connection = do
	load_all_tasks connection <&> \case
		(od : ods, st : sts, tt : tts) -> Zipper
			[("OVERDUE tasks", Zipper [] od ods)]
			("Tasks still in TODO", Zipper [] st sts)
			[("Tasks needed TODO today", Zipper [] tt tts)]
		(od : ods, st : sts, []) -> Zipper 
			[("OVERDUE tasks", Zipper [] od ods)] 
			("Tasks still in TODO", Zipper [] st sts) []
		(od : ods, [], tt : tts) -> Zipper 
			[("OVERDUE tasks", Zipper [] od ods)] 
			("Tasks needed TODO today", Zipper [] tt tts) []
		([], st : sts, tt : tts) -> Zipper
			[("Tasks still in TODO", Zipper [] st sts)]
			("Tasks needed TODO today", Zipper [] tt tts) []
		([], st : sts, []) -> Zipper [] ("Tasks still in TODO", Zipper [] st sts) []
		(od : ods, [], []) -> Zipper [] ("OVERDUE tasks", Zipper [] od ods) []
		([], [], tt : tts) -> Zipper [] ("Tasks needed TODO today", Zipper [] tt tts) []

todo_today =
	"SELECT tasks.id, status, task_event_priority, title, \
	\strftime('%H:%M', start, 'unixepoch', 'localtime'), \
	\IFNULL(strftime('%H:%M', stop, 'unixepoch', 'localtime'), '.....') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start >= " <> start_of_today <> " AND IFNULL(stop <= " <> end_of_today <> ", 1) \
	\ORDER BY status, task_event_priority, start;"

main = do
	connection <- open "facts.db"
	hSetBuffering stdin NoBuffering
	hSetEcho stdin False
	putStr "\ESC[?25l"

	--query_ @Task connection todo_today >>= \case
	--	t : ts -> print_focused_tasks ("TODAY", Zipper [] t ts)

	refresh_tasks connection >>= evalStateT
		(forever $ display *> (lift getChar >>= keystroke connection))
	--flip evalStateT (Zipper (overdue <> still) (head today) (tail today))
