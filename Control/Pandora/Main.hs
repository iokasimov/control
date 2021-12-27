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
import Control.Pandora.SQLite (today_timeline, today_tasks, today_timesheet, update_task_status, shift_task_bounds, start_objective_event, stop_objective_event)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal, line, focused, record, bold, negative, underlined, heading)
import Control.Pandora.Utils (keystroke, to_list, to_zipper)

type Picker a = Tape List a

type Timeline = List Event
type Timesheet = List Amount

type Overview = Timeline :*: Timesheet :*: Maybe (Picker Task)

type Frames = Overview :+: Picker Objective

type TUI = Provision Connection :> State Frames :> Maybe :> IO

display :: Frames -> IO ()
display (Option (timeline :*: timesheet :*: Just tasks)) = void ! do
	refresh_terminal
	putStrLn . heading . line . underlined ! "Timeline for today"
	putStrLn . line . show <<- timeline
	putStrLn . heading . line . underlined ! "Timesheet for today"
	putStrLn . line . show <<- timesheet
	putStrLn . heading . line . underlined ! "Tasks for today"
	putStrLn . record . show <<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . focused . show <<-<<- view (sub @Root) tasks
	putStrLn . record . show <<-<<- view (sub @Right) tasks
display (Adoption picker) = void ! do
	refresh_terminal
	putStrLn . heading . line . underlined ! "Pick an objective:"
	putStrLn . record . show <<-<<- (Reverse <-|- view (sub @Left) picker)
	putStrLn . focused . show <<-<<- view (sub @Root) picker
	putStrLn . record . show <<-<<- view (sub @Right) picker

move :: forall direction element . Morphed # Rotate direction # Tape List # Maybe <::> Tape List => Tape List element -> Tape List element
move z = resolve @(Tape List element) identity z # run (rotate @direction z)

handle :: ASCII -> TUI ()
handle (Letter Lower R) = void ! replace @Frames . Option =<< adapt . load_facts =<< provided @Connection
handle (Letter Lower J) = void # modify @Frames (over (perhaps @(Picker Task)) (move @Right <-|-) :*: move @Right <-|-<-|-)
handle (Letter Lower K) = void # modify @Frames (over (perhaps @(Picker Task)) (move @Left <-|-) :*: move @Left <-|-<-|-)
handle (Letter Upper G) = pass -+- (change_status_in_db .-*-*- adapt . confirmation) GONE
handle (Letter Upper T) = pass -+- (change_status_in_db .-*-*- adapt . confirmation) TODO
handle (Letter Upper D) = pass -+- (change_status_in_db .-*-*- adapt . confirmation) DONE
-- handle (Letter Upper I) = identity =<< insert_new_event <-|- provided @Connection
	-- <-*- (adapt =<< zoom @Frames # perhaps @Overview >>> perhaps @(Picker Task) >>> sub @Root >>> access @Task >>> access @(ID Objective) # overlook current)
handle (Letter Upper I) = void ! replace @Frames . Adoption =<< adapt . load_objectives =<< provided @Connection
handle (Control ESC) = void ! replace @Frames . Option =<< adapt . load_facts =<< provided @Connection
handle (Letter Upper O) = identity =<< finish_event <-|- provided @Connection
	<-*- (adapt =<< zoom @Frames # perhaps @Overview >>> perhaps @(Picker Task) >>> sub @Root >>> access @Task >>> access @(ID Objective) # overlook current)
handle (Letter Upper S) = pass -+- shift_task
-- It works exclusively for clocking in objectives and switches back to overview frame
handle (Control VT) = handle (Control ESC) .-*- (identity =<< insert_new_event <-|- provided @Connection
	<-*- (adapt =<< zoom @Frames # perhaps @(Picker Objective) >>> sub @Root >>> access @Objective >>> access @(ID ()) # overlook current))
handle _ = point ()

insert_new_event :: Connection -> ID () -> TUI ()
insert_new_event connection = adapt . execute connection start_objective_event . Only

finish_event :: Connection -> ID Objective -> TUI ()
finish_event connection = adapt . execute connection stop_objective_event . Only

shift_task :: TUI ()
shift_task = identity =<< shift_task_row <-|- provided @Connection
	<-*- (adapt =<< zoom @Frames # perhaps @Overview >>> perhaps @(Picker Task) >>> sub @Root >>> access @Task >>> access @(ID ()) # overlook current)
	<-*- adapt (shift_unit =<< keystroke .-*- message) where

	shift_unit :: ASCII -> Maybe :> IO := Int
	shift_unit (Letter Upper H) = point 3600
	shift_unit (Letter Upper D) = point 86400
	shift_unit (Letter Upper W) = point 604800
	shift_unit _ = nothing

	message :: Maybe :> IO := ()
	message = adapt . putStrLn . heading . line . negative
		! "Choose a time unit to shift the task on. (Hour/Day/Week)"

	shift_task_row :: Connection -> ID () -> Int -> TUI ()
	shift_task_row connection id shift = adapt # execute
		connection shift_task_bounds (shift, shift, id)

confirmation :: Status -> Maybe :> IO := ()
confirmation new = recognize =<< keystroke .-*- message where

	recognize :: ASCII -> Maybe :> IO := ()
	recognize (Letter Upper N) = nothing
	recognize (Letter Upper Y) = point ()
	recognize _ = confirmation new

	message :: Maybe :> IO := ()
	message = adapt . putStrLn . heading . line . negative
		! "Are you sure you want to mark this task as [" + show new + "]? (Yes/No)"

-- Before we update row in DB, new status should already be set
-- It would be better if we just get status and ID for the task right from zoomed state
change_status_in_db :: Status -> TUI ()
change_status_in_db new = identity =<< update_task_row <-|- provided @Connection
	<-*- (adapt =<< zoom @Frames # perhaps @Overview >>> perhaps @(Picker Task) >>> sub @Root >>> access @Task >>> access @(ID ()) # overlook current)
	<-*- (adapt =<< zoom @Frames # perhaps @Overview >>> perhaps @(Picker Task) >>> sub @Root >>> access @Task >>> access @Status # overlook (replace new)) where

	update_task_row :: Connection -> ID () -> Status -> TUI ()
	update_task_row connection id status = adapt # execute connection update_task_status (status, unid id)

eventloop :: TUI ()
eventloop = forever_ ! handle =<< adapt keystroke .-*- (adapt . display =<< current)

load_facts :: Connection -> IO Overview
load_facts connection = (\timeline timeshet tasks -> timeline :*: timeshet :*: tasks)
	<-|- (to_list <-|- query_ connection today_timeline)
	<-*- (to_list <-|- query_ connection today_timesheet)
	<-*- (to_zipper . to_list <-|- query_ connection today_tasks)

load_objectives :: Connection -> Maybe :> IO := Picker Objective
load_objectives connection = unite # to_zipper . to_list <-|- query_ connection "SELECT * FROM objectives;"

main = do
	connection <- open "facts.db"
	prepare_terminal
	facts <- load_facts connection
	run (eventloop ! connection ! Option facts)
	-- Just objectives <- to_zipper . to_list <-|- query_ connection "SELECT * FROM objectives;"
	-- run (eventloop ! connection ! Adoption objectives)
