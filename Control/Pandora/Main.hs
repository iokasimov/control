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
import "base" System.IO (putStrLn)
import "sqlite-simple" Database.SQLite.Simple (Connection, FromRow, Query, Only (Only), open, query_, execute)

import Control.Pandora.Entity.ID (ID (unid))
import Control.Pandora.Entity.Objective (Objective)
import Control.Pandora.Entity.Amount (Amount)
import Control.Pandora.Entity.Event (Event)
import Control.Pandora.Entity.Task (Task, Status (TODO, DONE, GONE))
import Control.Pandora.SQLite (today_timeline, today_tasks, today_timesheet, update_task_status, start_objective_event, stop_objective_event)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal, line, focused, record, bold, negative, underlined, heading)
import Control.Pandora.Utils (keystroke, to_list, to_zipper)

type Timeline = List Event
type Timesheet = List Amount
type Tasks = Tape List Task
type Facts = Timeline :*: Timesheet :*: Maybe Tasks

type TUI = Provision Connection :> State Facts :> Maybe :> IO

display :: Facts -> IO ()
display (timeline :*: timesheet :*: Just tasks) = void $ do
	refresh_terminal
	putStrLn . heading . line . underlined $ "Timeline for today"
	putStrLn . line . show <<- timeline
	putStrLn . heading . line . underlined $ "Timesheet for today"
	putStrLn . line . show <<- timesheet
	putStrLn . heading . line . underlined $ "Tasks for today"
	putStrLn . record . show -<<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . focused . show -<<-<<- view (sub @Root) tasks
	putStrLn . record . show -<<-<<- view (sub @Right) tasks

handle :: ASCII -> TUI ()
handle (Letter Lower R) = void $ replace =<< adapt . load_facts =<< provided
handle (Letter Lower J) = adapt # navigate @Right
handle (Letter Lower K) = adapt # navigate @Left
handle (Letter Upper G) = pass -+- (change_status_in_db -*-*- adapt . confirmation) GONE
handle (Letter Upper T) = pass -+- (change_status_in_db -*-*- adapt . confirmation) TODO
handle (Letter Upper D) = pass -+- (change_status_in_db -*-*- adapt . confirmation) DONE
handle (Letter Upper I) = identity =<< insert_new_event <-|- provided
	<-*- (adapt =<< zoom @Facts # perhaps @Tasks >>> sub @Root >>> access @Task >>> access @(ID Objective) # overlook current)
handle (Letter Upper O) = identity =<< finish_event <-|- provided
	<-*- (adapt =<< zoom @Facts # perhaps @Tasks >>> sub @Root >>> access @Task >>> access @(ID Objective) # overlook current)
handle _ = point ()

insert_new_event :: Connection -> ID Objective -> TUI ()
insert_new_event connection = adapt . execute connection start_objective_event . Only

finish_event :: Connection -> ID Objective -> TUI ()
finish_event connection = adapt . execute connection stop_objective_event . Only

confirmation :: Status -> Maybe :> IO := ()
confirmation new = recognize =<< keystroke -*- message where

	recognize :: ASCII -> Maybe :> IO := ()
	recognize (Letter Upper N) = nothing
	recognize (Letter Upper Y) = point ()
	recognize _ = confirmation new

	message :: Maybe :> IO := ()
	message = adapt . putStrLn . heading . line . negative
		$ "Are you sure you want to mark this task as [" + show new + "]? (Yes/No)"

-- Before we update row in DB, new status should already be set
-- It would be better if we just get status and ID for the task right from zoomed state
change_status_in_db :: Status -> TUI ()
change_status_in_db new = identity =<< update_task_row <-|- provided
	<-*- (adapt =<< zoom @Facts # perhaps @Tasks >>> sub @Root >>> access @Task >>> access @(ID ()) # overlook current)
	<-*- (adapt =<< zoom @Facts # perhaps @Tasks >>> sub @Root >>> access @Task >>> access @Status # overlook (replace new)) where

	update_task_row :: Connection -> ID () -> Status -> TUI ()
	update_task_row connection id status = adapt $ execute connection update_task_status (status, unid id)

navigate :: forall direction . Morphed # Rotate direction # Tape List # Maybe <::> Tape List => State Facts ()
navigate = void $ zoom @Facts # access @(Maybe Tasks) $ overlook . overlook $ modify move where

	move :: Tasks -> Tasks
	move z = resolve @Tasks identity z # run (rotate @direction z)

eventloop :: TUI ()
eventloop = forever_ $ handle =<< adapt keystroke -*- (adapt . display =<< current)

load_facts :: Connection -> IO Facts
load_facts connection = (\timeline timeshet tasks -> timeline :*: timeshet :*: tasks)
	<-|- (to_list <-|- query_ connection today_timeline)
	<-*- (to_list <-|- query_ connection today_timesheet)
	<-*- (to_zipper . to_list <-|- query_ connection today_tasks)

main = do
	connection <- open "facts.db"
	prepare_terminal
	facts <- load_facts connection
	run $ eventloop ! connection ! facts
