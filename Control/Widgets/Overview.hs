{-# LANGUAGE NoImplicitPrelude #-}
module Control.Widgets.Overview where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Char (Char)
import "base" Data.Int (Int)
import "base" Data.String (String)
import "base" Data.List (reverse)
import "base" Text.Show (show)
import "base" System.IO (putStrLn, putStr, putChar)
import "sqlite-simple" Database.SQLite.Simple (Connection, FromRow, Query, Only (Only), open, query_, query, execute, execute_)

import Control.Entity.ID (ID (unid))
import Control.Entity.Objective (Objective)
import Control.Entity.Amount (Amount)
import Control.Entity.Event (Event)
import Control.Entity.Task (Task, Status (TODO, DONE, GONE))
import Control.Engine.SQLite (today_timeline, today_tasks, today_timesheet, update_task_status, shift_task_bounds, start_objective_event, stop_all_objective_events)
import Control.Widgets.Search (run_search)
import Control.Widgets.Components.Picker (Picker)
import Control.TUI (prepare_terminal, refresh_terminal, line, focused, record, bold, negative, underlined, heading)
import Control.Utils (keystroke, to_list, to_zipper, letter_to_char)

type Timeline = List Event
type Timesheet = List Amount

type Overview = Timeline :*: Timesheet :*: Maybe (Picker Task)
type TUI = Provision Connection :> State Overview :> Maybe :> IO

display :: Overview -> IO ()
display (timeline :*: timesheet :*: Just (Turnover tasks)) = void <-- do
	refresh_terminal
	putStrLn . heading . line . underlined <-- "Timeline for today"
	putStrLn . line . show <-/- timeline
	putStrLn . heading . line . underlined <-- "Timesheet for today"
	putStrLn . line . show <-/- timesheet
	putStrLn . heading . line . underlined <-- "Tasks for today"
	putStrLn . record . show <-/-- view <--- sub @(Left Branch) <--- view <-- sub @Rest <-- tasks
	putStrLn . focused . show <-/- view <-- sub @Root <-- tasks
	putStrLn . record . show <-/-- view <--- sub @(Right Branch) <--- view <-- sub @Rest <-- tasks

-- Here is the question: how to adapt `State ls :> Maybe` to `State bg :> Maybe`?

handle :: ASCII -> TUI ()
handle (Letter Lower R) = void <--- adapt . set @State =<< adapt . load_overview_facts =<< provided @Connection
handle (Letter Lower J) = adapt . void <----- zoom
	<---- perhaps @(Picker Task) @Overview
	<---- overlook <--- modify @State @(Picker Task) <-- rotate @Right
handle (Letter Lower K) = adapt . void <----- zoom
	<---- perhaps @(Picker Task) @Overview
	<---- overlook <--- modify @State @(Picker Task) <-- rotate @Left
handle (Letter Upper G) = pass -+- (change_status_in_db .-*-*- adapt . confirmation) GONE
handle (Letter Upper T) = pass -+- (change_status_in_db .-*-*- adapt . confirmation) TODO
handle (Letter Upper D) = pass -+- (change_status_in_db .-*-*- adapt . confirmation) DONE
handle (Letter Upper I) = handle (Letter Lower R)
	.-*--- identity ===<< insert_new_event
		<-|- provided @Connection
		<-*- adapt . run_search =<< provided @Connection
handle (Letter Upper O) = identity ===<< finish_all_events <-|- provided @Connection
handle (Letter Upper S) = pass -+- shift_task
handle _ = point ()

insert_new_event :: Connection -> Objective -> TUI ()
insert_new_event connection = adapt
	. execute connection start_objective_event . Only . attached

finish_all_events :: Connection -> TUI ()
finish_all_events connection = adapt
	<-- execute_ connection stop_all_objective_events

shift_task :: TUI ()
shift_task = identity ======<< shift_task_row
	<-|---- provided @Connection
	<-*---- adapt ====<< adapt <---- zoom @Overview
		<--- perhaps @(Picker Task) >>> sub @Root >>> access @(ID ())
		<--- overlook <-- current @(ID ())
	<-*---- adapt <----- shift_unit
		===<< keystroke .-*- message where

	shift_unit :: ASCII -> Maybe :> IO >>> Int
	shift_unit (Letter Upper H) = point 3600
	shift_unit (Letter Upper D) = point 86400
	shift_unit (Letter Upper W) = point 604800
	shift_unit _ = nothing

	message :: Maybe :> IO >>> ()
	message = adapt . putStrLn . heading . line . negative
		<-- "Choose a time unit to shift the task on. (Hour/Day/Week)"

	shift_task_row :: Connection -> ID () -> Int -> TUI ()
	shift_task_row connection id shift = adapt <-- execute
		connection shift_task_bounds (shift, shift, id)

confirmation :: Status -> Maybe :> IO >>> ()
confirmation new = recognize =<< keystroke .-*- message where

	recognize :: ASCII -> Maybe :> IO >>> ()
	recognize (Letter Upper N) = nothing
	recognize (Letter Upper Y) = point ()
	recognize _ = confirmation new

	message :: Maybe :> IO >>> ()
	message = adapt . putStrLn . heading . line . negative
		<-- "Are you sure you want to mark this task as [" + show new + "]? (Yes/No)"

-- Before we update row in DB, new status should already be set
-- It would be better if we just get status and ID for the task right from zoomed state
change_status_in_db :: Status -> TUI ()
change_status_in_db new = identity ===<< update_task_row <-|- provided @Connection
	<-*- (adapt ====<< adapt <---- zoom <--- perhaps @(Picker Task) @Overview >>> sub @Root >>> access @(ID ()) <--- get @State)
	<-*- (adapt ====<< adapt <---- zoom <--- perhaps @(Picker Task) @Overview >>> sub @Root >>> access @Status <--- overlook <-- set @State new) where

	update_task_row :: Connection -> ID () -> Status -> TUI ()
	update_task_row connection id status = adapt
		<-- execute connection update_task_status (status, unid id)

run_overview :: TUI ()
run_overview = loop
	<---- handle =<< adapt keystroke
		.-*- adapt . display =<< current @Overview

load_overview_facts :: Connection -> IO Overview
load_overview_facts connection = (\timeline timeshet tasks -> timeline :*: timeshet :*: tasks)
	<-|---- to_list <-|- query_ connection today_timeline
	<-*---- to_list <-|- query_ connection today_timesheet
	<-*---- Turnover <-|-|- to_zipper . to_list <-|- query_ connection today_tasks

load_objectives :: Connection -> Maybe :> IO >>> Picker Objective
load_objectives connection = unite
	<------ Turnover <-|-|- to_zipper . to_list
		<-|- query_ connection "SELECT * FROM objectives;"
