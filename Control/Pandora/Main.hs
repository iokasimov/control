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
	display_section False -<<-<<- (Reverse <-|- view (sub @Left) sections)
	display_section True -<<-<<- view (sub @Root) sections
	display_section False -<<-<<- view (sub @Right) sections

display_section :: Boolean -> Section -> IO ()
display_section active (title :*: tasks) = void $ do
	putStrLn # show_title active title
	putStrLn . show_task False -<<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . show_task active -<<-<<- view (sub @Root) tasks
	putStrLn . show_task False -<<-<<- view (sub @Right) tasks

show_title False title = "\n   \ESC[4m" + title + "\ESC[0m"
show_title True title = "\n + \ESC[1m\ESC[4m" + title + "\ESC[0m"

refresh' :: State (Zipper List Section) :> IO := ()
refresh' = adapt . display =<< current

type TUI = State Section :> IO

--keystroke :: Char -> TUI ()
--keystroke 'j' = adapt # navigation @Right
--keystroke 'k' = adapt # navigation @Left

navigation :: Char -> State (Zipper List Section) ()
navigation 'j' = zoom @(Zipper List Section) (access @Cursor . access @Section . sub @Root) (inner @Right)
navigation 'k' = zoom @(Zipper List Section) (access @Cursor . access @Section . sub @Root) (inner @Left)
navigation 'l' = outer @Right
navigation 'h' = outer @Left

inner :: forall direction . (Morphed (Rotate direction) (Zipper List) (Maybe <:.> Zipper List)) => State (Zipper List Task) ()
inner = void . modify $ \z -> resolve @Cursor identity z # run (rotate @direction z)

outer :: forall direction . (Morphed (Rotate direction) (Zipper List) (Maybe <:.> Zipper List)) => State (Zipper List Section) ()
outer = void . modify $ \z -> resolve @(Zipper List Section) identity z # run (rotate @direction z)

eventloop = (eventloop !.) =<< adapt . navigation =<< (adapt getChar !.) =<< refresh'

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
