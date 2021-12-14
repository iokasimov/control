{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Char (Char)
import "base" Data.Int (Int)
import "base" Data.String (String)
import "base" Text.Show (show)
import "base" System.IO (getChar, putStrLn)
import "sqlite-simple" Database.SQLite.Simple (Connection, FromRow, Query, Only (Only), open, query_, execute)

import Control.Pandora.Entity.ID (ID (unid))
import Control.Pandora.Entity.Objective (Objective)
import Control.Pandora.Entity.Event (Event)
import Control.Pandora.Entity.Task (Task, Status (TODO, DONE, GONE))
import Control.Pandora.SQLite (today_events, today_tasks, update_task_status, start_objective_event, stop_objective_event)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal, line, focused, record, bold, negative, underlined, heading)
import Control.Pandora.Utils (castASCII, list_to_list)

type Clocked = Maybe (ID Event)
type Events = List Event
type Tasks = Tape List Task
type Facts = Clocked :*: Events :*: Maybe Tasks

type TUI = Environment Connection :> State Facts :> Maybe :> IO

display :: Facts -> IO ()
display (_ :*: events :*: Just tasks) = void $ do
	refresh_terminal
	putStrLn . heading . line . underlined $ "Events for today"
	putStrLn . line . show <<- events
	putStrLn . heading . line . underlined $ "Tasks for today"
	putStrLn . record . show -<<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . focused . show -<<-<<- view (sub @Root) tasks
	putStrLn . record . show -<<-<<- view (sub @Right) tasks

refresh :: TUI ()
refresh = adapt . display =<< current

cursor :: (Stateful Facts t, Optional t, Monad (->) t) => State Task r -> t r
cursor x = point . extract =<< adapt =<< zoom @Facts # perhaps @Tasks
	# overlook (zoom @Tasks # access @Task . sub @Root # overlook x)

handle :: ASCII -> TUI ()
handle (Letter Lower R) = void $ replace =<< adapt . load_facts =<< env
handle (Letter Lower J) = adapt # navigate @Right
handle (Letter Lower K) = adapt # navigate @Left
handle (Letter Upper G) = pass -+- (change_status_in_db -*-*- confirmation) GONE
handle (Letter Upper T) = pass -+- (change_status_in_db -*-*- confirmation) TODO
handle (Letter Upper D) = pass -+- (change_status_in_db -*-*- confirmation) DONE
handle (Letter Upper I) = clock_task_in
handle (Letter Upper O) = clock_task_out
handle c = point ()

confirmation :: Status -> TUI ()
confirmation new = adapt choice -*- adapt dialog where

	choice :: Maybe :> IO := ()
	choice = recognize =<< keystroke

	recognize :: ASCII -> Maybe :> IO := ()
	recognize (Letter Upper N) = nothing
	recognize (Letter Upper Y) = point ()
	recognize _ = choice

	dialog :: State Facts :> Maybe :> IO := ()
	dialog = message =<< title

	title :: State Facts :> Maybe :> IO := String
	title = cursor $ zoom @Task # access @String # extract <-|- overlook current

	message :: String -> State Facts :> Maybe :> IO := ()
	message task = adapt . putStrLn . heading . line . negative
		$ "Are you sure you want to mark this task as [" + show new + "]? (Yes/No)"

-- Before we update row in DB, new status should already be set
-- It would be better if we just get status and ID for the task right from zoomed state
change_status_in_db :: Status -> TUI ()
change_status_in_db new = identity =<< update_task_row <-|- env 
	<-*- cursor (zoom @Task # access @(ID ()) # unid . extract <-|- overlook (current @(ID ())))
	<-*- cursor (zoom @Task # access @Status # extract <-|- overlook (replace @Status new)) where

	update_task_row :: Connection -> Int -> Status -> TUI ()
	update_task_row connection id status = adapt $ execute connection update_task_status (status, id)

clock_task_in = identity =<< insert_new_event <-|- env <-*- focused_obj_id where

	focused_obj_id :: TUI := ID Objective
	focused_obj_id = cursor $ zoom @Task # access @(ID Objective) # extract <-|- overlook current

	insert_new_event :: Connection -> ID Objective -> TUI ()
	insert_new_event connection obj_id = adapt $ execute connection start_objective_event $ Only obj_id

clock_task_out = identity =<< finish_event <-|- env <-*- focused_obj_id where

	focused_obj_id :: TUI := ID Objective
	focused_obj_id = cursor $ zoom @Task # access @(ID Objective) # extract <-|- overlook current

	finish_event :: Connection -> ID Objective -> TUI ()
	finish_event connection obj_id = adapt $ execute connection stop_objective_event $ Only obj_id

navigate :: forall direction . Morphed # Rotate direction # Tape List # Maybe <:.> Tape List => State Facts ()
navigate = void $ zoom @Facts # access @(Maybe Tasks) $ overlook . overlook $ modify move where

	move :: Tape List Task -> Tape List Task
	move z = resolve @(Tape List Task) identity z # run (rotate @direction z)

keystroke :: Maybe :> IO := ASCII
keystroke = unite # castASCII <-|- getChar

eventloop :: TUI ()
eventloop = forever_ $ handle =<< adapt keystroke -*- refresh

load_facts :: Connection -> IO Facts
load_facts connection = (\es ts -> Nothing :*: es :*: ts) <-|- load_today_events <-*- load_today_tasks where

	load_today_events :: IO Events
	load_today_events = to_list <-|- from_db today_events

	load_today_tasks :: IO (Maybe Tasks)
	load_today_tasks = to_zipper . to_list <-|- from_db today_tasks

	from_db :: FromRow row => Query -> IO [row]
	from_db = query_ connection

	to_list = list_to_list # TU Nothing
	to_zipper = run . into @(Tape List)

main = do
	connection <- open "facts.db"
	prepare_terminal
	facts <- load_facts connection
	run $ eventloop ! connection ! facts
