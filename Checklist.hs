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

type Task = (Int, Int, String, String, String)

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
	focus . snd . focus <$> get >>= \(task_id, status, title, start, stop) -> do
		lift $ execute connection update_task_status (0 :: Int, task_id)
		modify $ \(Zipper bs (title, Zipper bbs x ffs) fs) ->
			Zipper bs (title, Zipper bbs (task_id, 0, title, start, stop) ffs) fs
keystroke connection 'C' = do
	focus . snd . focus <$> get >>= \(task_id, status, title, start, stop) -> do
		lift $ execute connection update_task_status (-1 :: Int, task_id)
		modify $ \(Zipper bs (title, Zipper bbs x ffs) fs) ->
			Zipper bs (title, Zipper bbs (task_id, -1, title, start, stop) ffs) fs
keystroke connection 'T' = do
	focus . snd . focus <$> get >>= \(task_id, status, title, start, stop) -> do
		lift $ execute connection update_task_status (1 :: Int, task_id)
		modify $ \(Zipper bs (title, Zipper bbs x ffs) fs) ->
			Zipper bs (title, Zipper bbs (task_id, 1, title, start, stop) ffs) fs
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
	putStrLn $ "   \ESC[7m" <> title <> "\ESC[0m\n"
	print_tasks $ Reverse bs
	print_focused_task x
	print_tasks $ fs

print_unfocused_tasks (title, Zipper bs x fs) = void $ do
	putStrLn $ "\n   " <> title <> "\n"
	print_tasks $ Reverse bs
	print_unfocused_task x
	print_tasks $ fs

print_tasks tasks = for tasks $ \t ->
	putStr "   " *> print_task t

print_focused_task t =
	putStr " * " *> print_task t

print_unfocused_task t =
	putStr "   " *> print_task t

print_task :: (Int, Int, String, String, String) -> IO ()
print_task (_, -2, title, start, stop) = do
	putStrLn $ "[\ESC[0mLATE\ESC[0m] " <> start <> " - " <> stop <> " " <> title
print_task (_, -1, title, start, stop) = do
	putStrLn $ "[\ESC[0mGONE\ESC[0m] " <> start <> " - " <> stop <> " " <> title
print_task (_, 0, title, start, stop) = do
	putStrLn $ "[\ESC[4mDONE\ESC[0m] " <> start <> " - " <> stop <> " " <> title
print_task (_, 1, title, start, stop) = do
	putStrLn $ "[\ESC[1mTODO\ESC[0m] " <> start <> " - " <> stop <> " " <> title

today_todo :: Query
today_todo =
	"SELECT tasks.id, status, title, strftime('%d.%m %H:%M', start, 'unixepoch', 'localtime'), IFNULL(strftime('%d.%m %H:%M', stop, 'unixepoch', 'localtime'), '.....') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start >= " <> start_of_today <> " AND IFNULL(stop <= " <> end_of_today <> ", 1) \
	\ORDER BY status;"

still_todo :: Query
still_todo =
	"SELECT tasks.id, 1, title, strftime('%d.%m %H:%M', start, 'unixepoch', 'localtime'), IFNULL(strftime('%d.%m %H:%M', stop, 'unixepoch', 'localtime'), '.....') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE start < " <> start_of_today <> " AND IFNULL(stop >= strftime('%s', 'now'), 1) AND status = 1;"

overdue :: Query
overdue =
	"SELECT tasks.id, -2, title, strftime('%d.%m %H:%M', start, 'unixepoch', 'localtime'), strftime('%d.%m %H:%M', stop, 'unixepoch', 'localtime') \
	\FROM tasks JOIN objectives on tasks.objective_id = objectives.id \
	\WHERE status = 1 and stop < strftime('%s', 'now') \
	\ORDER BY start;"

load_all_tasks connection = (,,)
	<$> query_ @Task connection overdue
	<*> query_ @Task connection still_todo
	<*> query_ @Task connection today_todo

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

main = do
	connection <- open "facts.db"
	hSetBuffering stdin NoBuffering
	hSetEcho stdin False
	putStr "\ESC[?25l"
	refresh_tasks connection >>= evalStateT
		(forever $ display *> (lift getChar >>= keystroke connection))
	--flip evalStateT (Zipper (overdue <> still) (head today) (tail today))
