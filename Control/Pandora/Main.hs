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
import "sqlite-simple" Database.SQLite.Simple (Connection, Query, open, query_, execute)

import Control.Pandora.Entity.ID (ID (unid))
import Control.Pandora.Entity.Event (Event)
import Control.Pandora.Entity.Task (Task, Status (TODO, DONE, GONE))
import Control.Pandora.SQLite (today_events, today_tasks, update_task_status)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal)
import Control.Pandora.Utils (castASCII, list_to_list)

show_event :: Event -> String
show_event (title :*: start :*: stop :*: total) =
	"   " +  "{" + start + " - " + stop + " => " + total + "} " + title

show_task :: Boolean -> Task -> String
show_task focused (_ :*: status :*: mode :*: oid :*: title :*: start :*: stop) =
	focused_mark focused + "[" + show status + "] " + show_task_boundaries mode start stop + title where

	focused_mark True = " * "
	focused_mark False = "   "

	show_task_boundaries 0 start [] = "{" + start + " - ....."
	show_task_boundaries 0 start stop = "{" + start + " - " + stop + "} "

display :: Maybe (ID Event) :*: Events :*: Maybe Tasks -> IO ()
display (_ :*: events :*: Just tasks) = void $ do
	refresh_terminal
	putStrLn $ "\n   \ESC[1m\ESC[4m" + "Events for today" + "\ESC[0m\n"
	putStrLn . show_event <<- events
	putStrLn $ "\n   \ESC[1m\ESC[4m" + "Tasks for today" + "\ESC[0m\n"
	putStrLn . show_task False -<<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . show_task True -<<-<<- view (sub @Root) tasks
	putStrLn . show_task False -<<-<<- view (sub @Right) tasks

refresh :: TUI ()
refresh = adapt . display =<< current

type Clocked = Maybe (ID Event)
type Events = List Event
type Tasks = Tape List Task
type Model = Clocked :*: Events :*: Maybe Tasks

type TUI = Environment Connection :> State Model :> Maybe :> IO

cursor :: (Stateful Model t, Optional t, Monad (->) t) => State Task r -> t r
cursor x = point . extract =<< adapt =<< zoom @Model # perhaps @Tasks # overlook (zoom @Tasks # access @Task . sub @Root # overlook x)

handle :: ASCII -> TUI ()
handle (Letter Lower R) = point ()
handle (Letter Lower J) = adapt # navigate @Right
handle (Letter Lower K) = adapt # navigate @Left
handle (Letter Upper G) = change_status_in_db GONE -*- confirmation GONE
handle (Letter Upper T) = change_status_in_db TODO -*- confirmation TODO
handle (Letter Upper G) = change_status_in_db DONE -*- confirmation DONE
handle c = point ()

confirmation :: Status -> TUI ()
confirmation new = adapt choice -*- adapt dialog where

	choice :: Maybe :> IO := ()
	choice = recognize =<< keystroke

	recognize :: ASCII -> Maybe :> IO := ()
	recognize (Letter Upper N) = nothing
	recognize (Letter Upper Y) = point ()
	recognize _ = choice

	dialog :: State Model :> Maybe :> IO := ()
	dialog = message =<< title

	title :: State Model :> Maybe :> IO := String
	title = cursor (zoom @Task # access @String # extract <-|- overlook current)

	message :: String -> State Model :> Maybe :> IO := ()
	message task = adapt . putStrLn
		$ "\n   \ESC[4m" + "Are you sure you want to mark \"" + task
			+ "\" as [\ESC[7m" + show new + "\ESC[27m]? (Yes/No)\ESC[24m\n"

change_status_in_state :: Status -> TUI Status
change_status_in_state new = cursor $ zoom @Task # access @Status # extract <-|- overlook (replace @Status new)

change_status_in_db :: Status -> TUI ()
change_status_in_db new = identity =<< update_task_row <-|- env 
	<-*- cursor (zoom @Task # access @(ID ()) # unid . extract <-|- overlook (current @(ID ())))
	<-*- cursor (zoom @Task # access @Status # extract <-|- overlook (replace @Status new))

-- Before we update row in DB, new status should already be set
-- It would be better if we just get status and ID for the task right from zoomed state
update_task_row :: Connection -> Int -> Status -> TUI ()
update_task_row connection id status = adapt $ execute connection update_task_status (status, id)

navigate :: forall direction . Morphed (Rotate direction) (Tape List) (Maybe <:.> Tape List) => State Model ()
navigate = void $ zoom @Model # access @(Maybe Tasks) $ overlook . overlook $ modify move where

	move :: Tape List Task -> Tape List Task
	move z = resolve @(Tape List Task) identity z # run (rotate @direction z)

keystroke :: Maybe :> IO := ASCII
keystroke = unite # castASCII <-|- getChar

eventloop :: TUI ()
eventloop = forever_ $ handle =<< adapt keystroke -*- refresh

main = do
	connection <- open "facts.db"
	tasks <- run . into @(Tape List) . list_to_list (TU Nothing) <-|- query_ @Task connection today_tasks
	events <- list_to_list (TU Nothing) <-|- query_ @Event connection today_events
	prepare_terminal
	run $ eventloop ! connection ! (Nothing :*: events :*: tasks)
