{-# LANGUAGE NoImplicitPrelude #-}
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
import "sqlite-simple" Database.SQLite.Simple (Connection, FromRow, Query, Only (Only), open, query_, query, execute, execute_)

import Control.Pandora.Widgets.Search (run_search)
import Control.Pandora.Widgets.Overview (run_overview, load_overview_facts)
import Control.Pandora.Widgets.Components.Picker (Picker)
import Control.Pandora.TUI (prepare_terminal)

main = do
	connection <- open "facts.db"
	prepare_terminal
	facts <- load_overview_facts connection
	run (run_overview ! connection ! facts)
