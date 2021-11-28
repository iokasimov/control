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
import "base" System.IO (getChar, putStrLn)
import "sqlite-simple" Database.SQLite.Simple (Query, open, query_)

import Control.Pandora.SQLite (today_tasks)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal)
import Control.Pandora.Utils (list_to_list)

type Task = (Int, Int, Int, String, String, String)

type Cursor = Zipper List Task

type Section = String :*: Cursor

show_task :: Boolean -> Task -> String
show_task focused (_, status, mode, title, start, stop) =
	focused_mark focused + show_task_status status + show_task_boundaries mode start stop + title where

	focused_mark True = " * "
	focused_mark False = "   "

	show_task_status (-2) = "[LATE] "
	show_task_status (-1) = "[GONE] "
	show_task_status 0 = "[DONE] "
	show_task_status 1 = "[TODO] "

	show_task_boundaries 0 ready [] = "[READY: " + ready + "] "
	show_task_boundaries 0 ready deadline = "[READY: " + ready + "] [DEADLINE: " + deadline + "] "
	show_task_boundaries 1 begin [] = "[BEGIN: " + begin + "] "
	show_task_boundaries 1 begin complete = "[BEGIN: " + begin + "] [COMPLETE: " + complete + "] "

display :: Zipper List Section -> IO ()
display sections = void $ do
	refresh_terminal
	display_section <<- sections

display_section :: Section -> IO ()
display_section (title :*: tasks) = void $ do
	putStrLn # "\n + \ESC[1m\ESC[4m" + title + "\ESC[0m"
	putStrLn . show_task False -<<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . show_task True -<<-<<- view (sub @Root) tasks
	putStrLn . show_task False -<<-<<- view (sub @Right) tasks

refresh :: State Section :> IO := ()
refresh = adapt . display_section =<< current

refresh' :: State (Zipper List Section) :> IO := ()
refresh' = adapt . display =<< current

type TUI = State Section :> IO

keystroke :: Char -> TUI ()
keystroke 'j' = adapt # navigation @Right
keystroke 'k' = adapt # navigation @Left

keystroke' :: Char -> State (Zipper List Section) ()
keystroke' 'j' = zoom @(Zipper List Section) (access @Section . sub @Root) (navigation @Right)
keystroke' 'k' = zoom @(Zipper List Section) (access @Section . sub @Root) (navigation @Left)

navigation :: forall direction . (Morphed (Rotate direction) (Zipper List) (Maybe <:.> Zipper List)) => State Section ()
navigation = void . zoom @Section (access @Cursor) . modify $ \z -> resolve @Cursor identity z # run (rotate @direction z)

eventloop = (eventloop !.) =<< adapt . keystroke' =<< (adapt getChar !.) =<< refresh'

load_tasks_zipper connection (title :*: q) =
	((:*:) title <-|-) . run . into @(Zipper List) . list_to_list empty <-|- query_ @Task connection q

queries :: Nonempty List := String :*: Query
queries = into @(Nonempty List) . vectorize
	$ (("Tasks for TODAY (1)" :: String) :*: today_tasks)
	:*: (("Tasks for TODAY (2)" :: String) :*: today_tasks)
	:*: (("Tasks for TODAY (3)" :: String) :*: today_tasks)
	:*: (("Tasks for TODAY (4)" :: String) :*: today_tasks)

sections :: Nonempty List (Maybe Section) -> Maybe :. Zipper List :. (:*:) String :. Zipper List := Task
sections (Construct (Just x) (Just xs)) = Just $ twosome (Identity x) (twosome empty $ into @List $ TU @Covariant @Covariant xs)
sections (Construct (Just x) Nothing) = Just $ twosome (Identity x) (twosome empty empty)
sections (Construct Nothing (Just xs)) = run $ (into @(Zipper List) # into @List (TU @Covariant @Covariant xs))
sections (Construct Nothing Nothing) = Nothing

main = do
	connection <- open "facts.db"
	today <- query_ @Task connection today_tasks
	let Just section = run . into @(Zipper List) # list_to_list empty today
	prepare_terminal
	Just ss <- sections <-|- load_tasks_zipper connection <<- queries
	--eventloop ! "Tasks for TODAY" :*: section
	eventloop ! ss
