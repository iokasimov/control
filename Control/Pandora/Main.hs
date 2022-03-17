{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "sqlite-simple" Database.SQLite.Simple (open)

import Control.Pandora.Widgets.Overview (run_overview, load_overview_facts)
import Control.Pandora.TUI (prepare_terminal)

main = do
	-- TODO: put path to a database file into configuration
	connection <- open "/Users/iokasimov/Dropbox/facts.db"
	prepare_terminal
	facts <- load_overview_facts connection
	run <-- run_overview <~ connection <~ facts
