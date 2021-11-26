{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Int (Int)
import "base" Data.String (String)
import "base" System.IO (getChar, putStrLn)
import "sqlite-simple" Database.SQLite.Simple (open, query_)

import Control.Pandora.SQLite (today_tasks)
import Control.Pandora.TUI (prepare_terminal, refresh_terminal)
import Control.Pandora.Utils (list_to_list)

type Task = (Int, Int, Int, String, String, String)

type Section = String :*: Zipper List Task

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

display :: Zipper List Task -> IO ()
display tasks = void $ do
	refresh_terminal
	putStrLn . show_task False -<<-<<- (Reverse <-|- view (sub @Left) tasks)
	putStrLn . show_task True -<<-<<- view (sub @Root) tasks
	putStrLn . show_task False -<<-<<- view (sub @Right) tasks

refresh :: State (Zipper List Task) :> IO := ()
refresh = adapt . display =<< current

main = do
	connection <- open "facts.db"
	today <- query_ @Task connection today_tasks
	let Just tasks = run . into @(Zipper List) # list_to_list today empty
	prepare_terminal
	refresh ! tasks
	--forever $ getChar >>= print
