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
import "base" Data.List (reverse)
import "base" Text.Show (show)
import "base" System.IO (putStrLn, putStr, putChar)
import "sqlite-simple" Database.SQLite.Simple (Connection, FromRow, Query, Only (Only), open, query_, query, execute)

import Control.Pandora.Entity.ID (ID (unid))
import Control.Pandora.Entity.Objective (Objective)
import Control.Pandora.Entity.Amount (Amount)
import Control.Pandora.Entity.Event (Event)
import Control.Pandora.Entity.Task (Task, Status (TODO, DONE, GONE))
import Control.Pandora.SQLite (today_timeline, today_tasks, today_timesheet, update_task_status, shift_task_bounds, start_objective_event, stop_objective_event)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal, line, focused, record, bold, negative, underlined, heading)
import Control.Pandora.Utils (keystroke, to_list, to_zipper, letter_to_char)

type Picker a = Tape List a
type Timeline = List Event
type Timesheet = List Amount

type Overview = Timeline :*: Timesheet :*: Maybe (Picker Task)
type TUI = Provision Connection :> State Overview :> Maybe :> IO

display :: Overview -> IO ()
display (timeline :*: timesheet :*: Just tasks) = void ! do
	refresh_terminal
	putStrLn . heading . line . underlined ! "Timeline for today"
	putStrLn . line . show <<- timeline
	putStrLn . heading . line . underlined ! "Timesheet for today"
	putStrLn . line . show <<- timesheet
	putStrLn . heading . line . underlined ! "Tasks for today"
	putStrLn . record . show <<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . focused . show <<-<<- view (sub @Root) tasks
	putStrLn . record . show <<-<<- view (sub @Right) tasks

move :: forall direction a . Morphed # Rotate direction # Tape List # Maybe <::> Tape List => Tape List a -> Tape List a
move z = resolve @(Tape List a) identity z # run (rotate @direction z)

handle :: ASCII -> TUI ()
handle (Letter Lower R) = void ! replace @Overview =<< adapt . load_facts =<< provided @Connection
handle (Letter Lower J) = adapt @TUI . void ! zoom @Overview @_ @(State Overview) # perhaps @(Picker Task) # overlook (modify @(Picker Task) (move @Right))
handle (Letter Lower K) = adapt @TUI . void ! zoom @Overview @_ @(State Overview) # perhaps @(Picker Task) # overlook (modify @(Picker Task) (move @Left))
handle (Letter Upper G) = pass -+- (change_status_in_db .-*-*- adapt . confirmation) GONE
handle (Letter Upper T) = pass -+- (change_status_in_db .-*-*- adapt . confirmation) TODO
handle (Letter Upper D) = pass -+- (change_status_in_db .-*-*- adapt . confirmation) DONE
handle (Letter Upper I) = handle (Letter Lower R) .-*- (identity =<< insert_new_event <-|- provided @Connection <-*- (adapt . (attached <-|-) . run_chooser =<< provided @Connection))

handle (Letter Upper O) = identity =<< finish_event <-|- provided @Connection
	<-*- (adapt =<< zoom @Overview # perhaps @(Picker Task) >>> sub @Root >>> access @Task >>> access @(ID Objective) # overlook current)
handle (Letter Upper S) = pass -+- shift_task
-- It works exclusively for clocking in objectives and switches back to overview frame
-- handle (Control VT) = handle (Control ESC) .-*- (identity =<< insert_new_event <-|- provided @Connection
	-- <-*- (adapt =<< zoom @Overview # perhaps @(Picker Objective) >>> sub @Root >>> access @Objective >>> access @(ID ()) # overlook current))
handle _ = point ()

insert_new_event :: Connection -> ID () -> TUI ()
insert_new_event connection = adapt . execute connection start_objective_event . Only

finish_event :: Connection -> ID Objective -> TUI ()
finish_event connection = adapt . execute connection stop_objective_event . Only

shift_task :: TUI ()
shift_task = identity =<< shift_task_row <-|- provided @Connection
	<-*- (adapt =<< zoom @Overview # perhaps @(Picker Task) >>> sub @Root >>> access @Task >>> access @(ID ()) # overlook current)
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
	<-*- (adapt =<< zoom @Overview # perhaps @(Picker Task) >>> sub @Root >>> access @Task >>> access @(ID ()) # overlook current)
	<-*- (adapt =<< zoom @Overview # perhaps @(Picker Task) >>> sub @Root >>> access @Task >>> access @Status # overlook (replace new)) where

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

type Texture = (List Letter :*: Maybe # Picker Objective) :+: Flip (:*:) (Maybe # Picker Objective) (List Letter)

type Chooser = Provision Connection :> State Texture :> Conclusion Objective :> IO

handle_chooser :: ASCII -> Chooser ()
handle_chooser (Control HT) = void . modify @Texture ! \case
	Option picker -> Adoption # Flip picker
	Adoption (Flip searcher) -> Option searcher
handle_chooser (Control VT) = choose_objective =<< current @Texture
handle_chooser key = update_objectives_list key =<< current @Texture

choose_objective :: Texture -> Chooser ()
choose_objective (Option (filter :*: Just picker)) = failure # extract picker
choose_objective _ = point ()

update_objectives_list :: ASCII -> Texture -> Chooser ()
update_objectives_list key (Option (filter :*: picker)) = void . replace @Texture . Option ! filter :*: (change_picker key <-|- picker)
update_objectives_list key (Adoption (Flip (filter :*: _))) = let new = change_filter key filter in
	void . replace @Texture . Adoption . Flip . (new :*:) =<< identity =<< (adapt . reload_objectives_by_filter % new) <-|- provided @Connection

-- TODO: think about caching with prefixed tree where key is a searching pattern
change_filter :: ASCII -> List Letter -> List Letter
change_filter (Letter _ letter) = item @Push letter
change_filter (Control DEL) = morph @Pop
change_filter _ = identity

change_picker :: ASCII -> Picker Objective -> Picker Objective
change_picker (Letter Lower J) = move @Right
change_picker (Letter Lower K) = move @Left
change_picker _ = identity

display_chooser :: Texture -> IO ()
display_chooser (Option (filter :*: picker)) = void ! do
	display_filter False filter
	resolve @(Picker Objective) ! display_picker True ! putStrLn (record "No objectives found") ! picker
display_chooser (Adoption (Flip (filter :*: picker))) = void ! do
	display_filter True filter
	resolve @(Picker Objective) ! display_picker False ! putStrLn (record "No objectives found") ! picker

display_filter :: Boolean -> List Letter -> IO ()
display_filter focus filter = void ! do
	putStrLn "" .-*- refresh_terminal
	putStr ! (focus ? focused ! record) "Search: \ESC[7m"
	putStrLn "\ESC[0m" .-*- putStrLn "" .-*- (putChar . letter_to_char <<- Reverse filter)

display_picker :: Boolean -> Picker Objective -> IO ()
display_picker focus objectives = void ! do
	putStrLn . record . show <<-<<- (Reverse <-|- view (sub @Left) objectives)
	putStrLn . (focus ? focused ! record) . show <<-<<- view (sub @Root) objectives
	putStrLn . record . show <<-<<- view (sub @Right) objectives

eventloop_chooser :: Chooser ()
eventloop_chooser = forever_ ! handle_chooser =<< adapt keystroke_chooser .-*- (adapt . display_chooser =<< current)

keystroke_chooser :: IO ASCII
keystroke_chooser = resolve @ASCII point keystroke_chooser =<< run keystroke

reload_objectives_by_filter :: Connection -> List Letter -> IO :. Maybe :. Picker := Objective
reload_objectives_by_filter connection pattern = let substring = reverse . show ! letter_to_char <-|- pattern in
	to_zipper . to_list <-|- query connection "SELECT * FROM objectives WHERE title LIKE '%' || ? || '%';" (Only substring)

run_chooser :: Connection -> IO Objective
run_chooser connection = do
	objectives <- reload_objectives_by_filter connection empty
	(\case { Failure obj -> obj } ) <-|- run (eventloop_chooser ! connection ! Option (empty :*: objectives))

main = do
	connection <- open "facts.db"
	prepare_terminal
	facts <- load_facts connection
	run (eventloop ! connection ! facts)
	-- objectives <- reload_objectives_by_filter connection empty
	-- run (eventloop_chooser ! connection ! Option (empty :*: objectives))
