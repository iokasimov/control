module Control.Interface.REPL where

import "base" Control.Monad (void)
import "base" Data.List (delete, find)
import "base" Text.Read (readMaybe)
import "joint" Control.Joint (over, _1, (<$$>))
import "haskeline" System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, outputStrLn)
import "sqlite-simple" Database.SQLite.Simple (Only (Only), open, query, query_, execute)
import "sqlite-simple" Database.SQLite.Simple.FromRow (FromRow (fromRow), field)
import "text" Data.Text (Text, unpack)
import "time" Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)

import qualified "text" Data.Text.IO as T (putStrLn)

import Control.Objective (Objective (Objective))
import Control.SQL.Query (start_objective_event, get_just_created_event, stop_objective_event, cancel_objective_event
	, today_time_query, today_tasks_query, tomorrow_tasks_query, all_unfinished_events, today_events_query, tomorrow_events_query)

data Event = Event Int Int Int (Maybe Int) deriving Show

instance FromRow Event where
	fromRow = Event <$> field <*> field <*> field <*> field

instance {-# OVERLAPS #-} FromRow (Objective, Int, String) where
	fromRow = (,,) <$> (Objective <$> field <*> field) <*> field <*> field

print_timeline :: [(String, String, String, String)] -> IO ()
print_timeline = void . traverse (putStrLn . prepare) where

	prepare (title, start, stop, amount) =
		" ├─ " <> start <> "-" <> stop <> " (" <> amount <> ") " <> title

print_task :: (Int, String, Maybe String, Maybe String) -> IO ()
print_task (-1, title, start, stop) = putStrLn $ " [CANCELED] ("
	<> maybe "..:.." id start <> " - " <> maybe "..:.." id stop <> ") " <> title <> " "
print_task (0, title, start, stop) = putStrLn $ " [DONE] ("
	<> maybe "..:.." id start <> " - " <> maybe "..:.." id stop <> ") " <> title <> " "
print_task (1, title, start, stop) = putStrLn $ " [TODO] ("
	<> maybe "..:.." id start <> " - " <> maybe "..:.." id stop <> ") " <> title <> " "

type Current = ([(Objective, Int, String)], Maybe Objective)

prompt :: InputT (StateT Current IO) (Maybe String)
prompt = snd <$> lift get >>= \case
	Nothing -> getInputLine "Mentat > "
	Just obj -> getInputLine $ "Mentat > " <> show obj <> " > "

--prompt' :: StateT Current IO String
--prompt' = snd <$> get >>= \case
--	Nothing -> lift $ putStr "Control > " *> hFlush stdout *> getLine
--	Just obj -> lift $ putStr ("Mentat > " <> show obj <> " > ") *> hFlush stdout *> getLine

loop :: InputT (StateT Current IO) ()
loop = prompt >>= \case
	Nothing -> pure ()
	Just "quit" -> outputStrLn "Bye!"
	Just "all" -> do
		connection <- lift . lift $ open "facts.db"
		lift . lift $ query_ @Objective connection "SELECT id, title from objectives" >>= void . traverse print
		loop
	Just "today total" -> do
		connection <- lift . lift $ open "facts.db"
		lift . lift $ query_ @(String, String) connection today_time_query >>= void . traverse print
		loop
	Just "today" -> do
		connection <- lift . lift $ open "facts.db"
		lift . lift $ query_ @(Int, String, Maybe String, Maybe String) connection today_tasks_query >>= void . traverse print_task
		lift . lift $ query_ @(String, String, String, String) connection today_events_query >>= print_timeline
		loop
	Just "tomorrow" -> do
		connection <- lift . lift $ open "facts.db"
		lift . lift $ query_ @(Int, String, Maybe String, Maybe String) connection tomorrow_tasks_query >>= void . traverse print_task
		lift . lift $ query_ @(String, String, String, String) connection tomorrow_events_query >>= print_timeline
		loop
	Just "focus" -> do
		connection <- lift . lift $ open "facts.db"
		-- Add new layer in monad transformer stack for handling errors
		Just objid <- (>>= readMaybe @Int) <$> getInputLine "Mentat > Objective ID ? "
		lift (lift $ query connection "SELECT id, title FROM objectives WHERE id = ?" $ Only objid) >>= \case
			[obj] -> lift $ modify (const (Just obj) <$>)
			[] -> wrong "No such an objective..."
		loop
	Just "unfocus" -> lift (modify (const Nothing <$>)) *> loop
	Just "start" -> snd <$> lift get >>= \case
		Nothing -> wrong "No focused objective..."
		Just (Objective id title) -> do
			connection <- lift . lift $ open "facts.db"
			lift . lift $ execute connection start_objective_event $ Only id
			r <- lift . lift $ query connection get_just_created_event $ Only id
			case r of
				[] -> wrong "ERROR: No such an event..."
				((event_id, start) : _) -> do
					lift $ modify $ over _1 ((Objective id title, event_id, start) :)
			loop
	Just "stop" -> lift get >>= \case
		(_, Nothing) -> wrong "No focused objective..."
		(clocking, Just obj) -> do
			case find (\(obj', event_id, start) -> obj == obj') clocking of
	 			Nothing -> wrong "Objective is not started"
				Just (Objective id _, event_id, start) -> do
					connection <- lift . lift $ open "facts.db"
					lift . lift $ execute connection stop_objective_event $ Only id
					lift . modify $ over _1 (delete (obj, event_id, start))
					loop
	Just "cancel" -> lift get >>= \case
		(_, Nothing) -> wrong "No focused objective..."
		(clocking, Just obj) -> do
			case find (\(obj', event_id, start) -> obj == obj') clocking of
	 			Nothing -> wrong "Objective is not started"
				Just (Objective id _, event_id, start) -> do
					connection <- lift . lift $ open "facts.db"
					lift . lift $ execute connection cancel_objective_event $ Only event_id
					lift . modify $ over _1 (delete (obj, event_id, start))
					loop
	Just _ -> wrong "Undefined command" *> loop

wrong description = do
	lift . lift . print $ "ERROR: " <> description
	loop
