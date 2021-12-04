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

import Control.Pandora.Task (Task, Status (TODO, DONE, GONE))
import Control.Pandora.SQLite (today_tasks, update_task_status)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal)
import Control.Pandora.Utils (list_to_list)

show_task :: Boolean -> Task -> String
show_task focused (_ :*: status :*: mode :*: title :*: start :*: stop) =
	focused_mark focused + "[" + show status + "] " + show_task_boundaries mode start stop + title where

	focused_mark True = " * "
	focused_mark False = "   "

	show_task_boundaries 0 ready [] = "{" + ready + " - ....."
	show_task_boundaries 0 ready deadline = "{" + ready + " - " + deadline + "} "
	show_task_boundaries 1 begin [] = "{" + begin + " - ....."
	show_task_boundaries 1 begin complete = "{" + begin + " - " + complete + "} "

display :: Tape List Task -> IO ()
display tasks = void $ do
	refresh_terminal
	putStrLn $ "\n   \ESC[1m\ESC[4m" + "Tasks for today" + "\ESC[0m\n"
	putStrLn . show_task False -<<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . show_task True -<<-<<- view (sub @Root) tasks
	putStrLn . show_task False -<<-<<- view (sub @Right) tasks

show_title False title = "\n   \ESC[4m" + title + "\ESC[0m"
show_title True title = "\n + \ESC[1m\ESC[4m" + title + "\ESC[0m"

refresh :: TUI ()
refresh = adapt . display =<< current

type TUI = Environment Connection :> State (Tape List Task) :> IO

instance Accessible field Task => Accessible field (Tape List Task) where
	access = access @field . access @Task . sub @Root

keystroke :: Char -> TUI ()
keystroke 'r' = point ()
keystroke 'j' = adapt # navigation @Right
keystroke 'k' = adapt # navigation @Left
keystroke 'D' = change_status DONE
keystroke 'T' = change_status TODO
keystroke 'G' = change_status GONE

change_status :: Status -> TUI ()
change_status new = identity =<< update_task_row <-|- env
	<-*- zoom @(Tape List Task) access (current @Int)
	<-*- zoom @(Tape List Task) access (replace new)

update_task_row :: Connection -> Int -> Status -> TUI ()
update_task_row connection id status = adapt $ execute connection update_task_status (status, id)

navigation :: forall direction . (Morphed (Rotate direction) (Tape List) (Maybe <:.> Tape List)) => State (Tape List Task) ()
navigation = void . modify $ \z -> resolve @(Tape List Task) identity z # run (rotate @direction z)

eventloop = (eventloop !.) =<< keystroke =<< (adapt getChar !.) =<< refresh

main = do
	connection <- open "facts.db"
	today <- query_ @Task connection today_tasks
	let Just tasks = run . into @(Tape List) # list_to_list empty today
	prepare_terminal
	(eventloop ! connection) ! tasks
