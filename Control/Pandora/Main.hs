{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Char (Char)
import "base" Data.Int (Int)
import "base" Data.String (String)
import "base" System.IO (getChar, putStrLn)
import "sqlite-simple" Database.SQLite.Simple (open, query_)

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

display :: Section -> IO ()
display section = void $ do
	refresh_terminal
	display_section section

display_section :: Section -> IO ()
display_section (title :*: tasks) = void $ do
	putStrLn # " + \ESC[1m\ESC[4m" + title + "\ESC[0m"
	putStrLn . show_task False -<<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . show_task True -<<-<<- view (sub @Root) tasks
	putStrLn . show_task False -<<-<<- view (sub @Right) tasks

refresh :: State Section :> IO := ()
refresh = adapt . display =<< current

keystroke :: Char -> State Section ()
keystroke 'j' = void . zoom @Section (access @Cursor) . modify
	$ \z -> resolve @Cursor identity z # run (rotate @Right z)
keystroke 'k' = void . zoom @Section (access @Cursor) . modify
	$ \z -> resolve @Cursor identity z # run (rotate @Left z)

eventloop = (eventloop !.) =<< adapt . keystroke =<< (adapt getChar !.) =<< refresh

main = do
	connection <- open "facts.db"
	today <- query_ @Task connection today_tasks
	let Just section = run . into @(Zipper List) # list_to_list today empty
	prepare_terminal
	eventloop ! ("Tasks for TODAY" :*: section)
