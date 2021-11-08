import "base" Control.Monad (void)
import "base" Data.List (delete, find)
import "base" Text.Read (readMaybe)
import "joint" Control.Joint (over, _1, (<$$>))
import "haskeline" System.Console.Haskeline
import "sqlite-simple" Database.SQLite.Simple
import "sqlite-simple" Database.SQLite.Simple.FromRow
import "text" Data.Text (Text, unpack)
import "time" Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)

import qualified "text" Data.Text.IO as T (putStrLn)

import Control.SQL.Query (start_objective_event, get_just_created_event, end_objective_event 
	, today_time_query, all_unfinished_events, timeline_today_events_query)

data Objective = Objective Int Text deriving (Eq, Show)

instance FromRow Objective where
	fromRow = Objective <$> field <*> field

objective_title :: Objective -> Text
objective_title (Objective _ title) = title

data Event = Event Int Int Int (Maybe Int) deriving Show

instance FromRow Event where
	fromRow = Event <$> field <*> field <*> field <*> field

instance {-# OVERLAPS #-} FromRow (Objective, Int, String) where
	fromRow = (,,) <$> (Objective <$> field <*> field) <*> field <*> field

event_start :: Event -> Int
event_start (Event _ _ start _) = start

data Period = Period String String deriving Show

instance FromRow Period where
	fromRow = Period <$> field <*> field

print_timeline :: [(Text, Text, Text, Text)] -> IO ()
print_timeline = void . traverse (T.putStrLn . prepare) where

	prepare (title, start, end, amount) =
		" ├─ " <> start <> "-" <> end <> " (" <> amount <> ") " <> title

type Current = ([(Objective, Int, String)], Maybe Objective)

currently_clocking_prompt :: [(Objective, Int, String)] -> IO ()
currently_clocking_prompt = void . traverse started where
	
	started :: (Objective, Int, String) -> IO ()
	started (Objective id title, _, start) = print
		$ "[" <> start <> " - ...] " <> unpack title 

prompt :: InputT (StateT Current IO) (Maybe String)
prompt = do
	--lift $ fst <$> get >>= lift . currently_clocking_prompt
	snd <$> lift get >>= \case
		Nothing -> getInputLine "Mentat > "
		Just obj -> getInputLine $ "Mentat > " <> unpack (objective_title obj) <> " > "

loop :: InputT (StateT Current IO) ()
loop = prompt >>= \case
	Nothing -> pure ()
	Just "quit" -> outputStrLn "Bye!"
	Just "all" -> do
		connection <- lift . lift $ open "facts.db"
		lift . lift $ query_ @Objective connection "SELECT * from objectives" >>= void . traverse print
		loop
	Just "today" -> do
		connection <- lift . lift $ open "facts.db"
		lift . lift $ query_ @Period connection today_time_query >>= void . traverse print
		loop
	Just "timeline" -> do
		connection <- lift . lift $ open "facts.db"
		lift . lift $ query_ @(Text, Text, Text, Text) connection timeline_today_events_query >>= print_timeline
		loop
	Just "focus" -> do
		connection <- lift . lift $ open "facts.db"
		-- Add new layer in monad transformer stack for handling errors
		Just objid <- (>>= readMaybe @Int) <$> getInputLine "Mentat > Objective ID ? "
		lift (lift $ query connection "SELECT * FROM objectives WHERE id = ?" $ Only objid) >>= \case
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
	Just "end" -> lift get >>= \case
		(_, Nothing) -> wrong "No focused objective..."
		(clocking, Just obj) -> do
			case find (\(obj', event_id, start) -> obj == obj') clocking of
	 			Nothing -> wrong "Objective is not started"
				Just (Objective id _, event_id, start) -> do
					connection <- lift . lift $ open "facts.db"
					lift . lift $ execute connection end_objective_event $ Only id
					lift . modify $ over _1 (delete (obj, event_id, start))
					loop
	Just _ -> wrong "Undefined command" *> loop

wrong description = do
	lift . lift . print $ "ERROR: " <> description
	loop

main = do
	connection <- open "facts.db"
	unfinished <- query_ connection all_unfinished_events
	void $ evalStateT (runInputT defaultSettings loop) (unfinished, Nothing)
