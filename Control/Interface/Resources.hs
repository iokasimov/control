{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Monoid ((<>))
import "base" Data.String (String)
import "base" System.IO (print, putStr, putStrLn, getLine)
import "base" Text.Show (show)
import "sqlite-simple" Database.SQLite.Simple (Connection, open, query_)

import Control.Engine.SQLite (all_todo_content)
import Control.Entity.Resource (Resource)
import Control.Entity.Status (Status)
import Control.TUI (prepare_terminal, refresh_terminal, record, prompt)
import Control.Utils (to_list)

load_resources :: Connection -> IO > List Resource
load_resources connection = to_list <-|- query_ connection all_todo_content

display :: List Resource -> IO ()
display resources = void <-- do
	refresh_terminal
	display_resource <-/- resources

display_resource :: Resource -> IO ()
display_resource (status :*: title :*: link :*: tags) =
	putStrLn (" \"" <> title <> "\"\n " <> link <> "\n " <> tags  <> "\n")


-- Trying to design a flow for adding a new resource.
-- Just for simplicity:
-- 1. We can add only articles
-- 1. We can add only existing tags
-- 1. We cannot go back and fix our input

	-- <-|- title
	-- <-*- link
	-- <-*- tags

main = void <-- do
	-- prepare_terminal
	-- connection <- open "/Users/iokasimov/Dropbox/facts.db"
	-- display =<< load_resources connection
	-- putStr "> "
	-- getLine -- .-*-
	prompt "> "
	getLine
