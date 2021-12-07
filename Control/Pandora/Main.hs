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

import Control.Pandora.Entity.Event (Event)
import Control.Pandora.Entity.Task (Task, Status (TODO, DONE, GONE))
import Control.Pandora.SQLite (today_events, today_tasks, update_task_status)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal)
import Control.Pandora.Utils (castASCII, list_to_list)

show_event :: Event -> String
show_event (title :*: start :*: stop :*: total) = "   " + show_event_boundaries start stop total + title where

	show_event_boundaries start stop total = "{" + start + " - " + stop + " => " + total + "} "

show_task :: Boolean -> Task -> String
show_task focused (_ :*: status :*: mode :*: title :*: start :*: stop) =
	focused_mark focused + "[" + show status + "] " + show_task_boundaries mode start stop + title where

	focused_mark True = " * "
	focused_mark False = "   "

	show_task_boundaries 0 ready [] = "{" + ready + " - ....."
	show_task_boundaries 0 ready deadline = "{" + ready + " - " + deadline + "} "
	show_task_boundaries 1 begin [] = "{" + begin + " - ....."
	show_task_boundaries 1 begin complete = "{" + begin + " - " + complete + "} "

display :: Events :*: Tasks -> IO ()
display (events :*: tasks) = void $ do
	refresh_terminal
	putStrLn $ "\n   \ESC[1m\ESC[4m" + "Events for today" + "\ESC[0m\n"
	putStrLn . show_event <<- events
	putStrLn $ "\n   \ESC[1m\ESC[4m" + "Tasks for today" + "\ESC[0m\n"
	putStrLn . show_task False -<<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . show_task True -<<-<<- view (sub @Root) tasks
	putStrLn . show_task False -<<-<<- view (sub @Right) tasks

confirm_change_status :: Status -> TUI (Maybe Status)
confirm_change_status status = choice =<< keystroke
	-*- (adapt . message =<< adapt title) -*- adapt refresh_terminal where

	message :: String -> IO ()
	message task = putStrLn
		$ "\n   \ESC[4m" + "Are you sure you want to mark \""
		+ task + "\" as [\ESC[7m" + show status + "\ESC[27m]? (Yes/No)\ESC[24m\n"

	title :: State Model String
	title = zoom @Model (access @String) current

	choice :: Maybe ASCII -> TUI (Maybe Status)
	choice (Just (Letter Upper Y)) = point # Just status
	choice (Just (Letter Upper N)) = point # Nothing
	choice _ = confirm_change_status status

show_title False title = "\n   \ESC[4m" + title + "\ESC[0m"
show_title True title = "\n + \ESC[1m\ESC[4m" + title + "\ESC[0m"

refresh :: TUI ()
refresh = adapt . display =<< current

type Events = List Event
type Tasks = Tape List Task
type Model = Events :*: Tasks

type TUI = Environment Connection :> State Model :> IO

instance Accessible field Task => Accessible field Tasks where
	access = access @field . access @Task . sub @Root

handle :: Maybe ASCII -> TUI ()
handle (Just (Letter Lower R)) = point ()
handle (Just (Letter Lower J)) = adapt # zoom @Model @Tasks @(State _) access (navigation @Right)
handle (Just (Letter Lower K)) = adapt # zoom @Model @Tasks @(State _) access (navigation @Left)
handle (Just (Letter Upper D)) = void $ (change_status_in_db <<-) =<< confirm_change_status DONE
handle (Just (Letter Upper T)) = void $ (change_status_in_db <<-) =<< confirm_change_status TODO
handle (Just (Letter Upper G)) = void $ (change_status_in_db <<-) =<< confirm_change_status GONE
handle c = point ()

change_status_in_db :: Status -> TUI ()
change_status_in_db new = identity =<< update_task_row <-|- env
	<-*- zoom @Model access (current @Int)
	<-*- zoom @Model access (replace new)

update_task_row :: Connection -> Int -> Status -> TUI ()
update_task_row connection id status = adapt $ execute connection update_task_status (status, id)

navigation :: forall direction . (Morphed (Rotate direction) (Tape List) (Maybe <:.> Tape List)) => State (Tape List Task) ()
navigation = void . modify $ \z -> resolve @(Tape List Task) identity z # run (rotate @direction z)

keystroke :: TUI (Maybe ASCII)
keystroke = castASCII <-|- adapt getChar

eventloop :: TUI ()
eventloop = forever_ $ handle =<< keystroke -*- refresh

main = do
	connection <- open "facts.db"
	Just tasks <- run . into @(Tape List) . list_to_list empty <-|- query_ @Task connection today_tasks
	events <- list_to_list empty <-|- query_ @Event connection today_events
	prepare_terminal
	eventloop ! connection ! (events :*: tasks)
