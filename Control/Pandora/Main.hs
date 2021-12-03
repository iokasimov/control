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
import "sqlite-simple" Database.SQLite.Simple (Connection, Query, open, query_, execute)

import Control.Pandora.SQLite (today_tasks, update_task_status)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal)
import Control.Pandora.Utils (list_to_list)

type Task = Int :*: Int :*: Int :*: String :*: String :*: String

show_task :: Boolean -> Task -> String
show_task focused (_ :*: status :*: mode :*: title :*: start :*: stop) =
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

display :: Zipper List Task -> IO ()
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

type TUI = Environment Connection :> State (Zipper List Task) :> IO

keystroke :: Char -> TUI ()
keystroke 'j' = adapt # navigation @Right
keystroke 'k' = adapt # navigation @Left

complete_task :: Connection -> Int -> IO ()
complete_task connection tid = execute connection update_task_status (1 :: Int, tid)

navigation :: forall direction . (Morphed (Rotate direction) (Zipper List) (Maybe <:.> Zipper List)) => State (Zipper List Task) ()
navigation = void . modify $ \z -> resolve @(Zipper List Task) identity z # run (rotate @direction z)

eventloop = (eventloop !.) =<< keystroke =<< (adapt getChar !.) =<< refresh

main = do
	connection <- open "facts.db"
	today <- query_ @Task connection today_tasks
	let Just tasks = run . into @(Zipper List) # list_to_list empty today
	prepare_terminal
	(eventloop ! connection) ! tasks
