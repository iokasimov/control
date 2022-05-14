{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Widgets.Search where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.List (reverse)
import "base" Text.Show (show)
import "base" System.IO (putStrLn, putStr, putChar)
import "sqlite-simple" Database.SQLite.Simple (Connection, Only (Only), query)

import Control.Entity.Objective (Objective)
import Control.Engine.SQLite ()
import Control.Widgets.Components.Picker (Picker)
import Control.TUI (refresh_terminal, focused, record)
import Control.Utils (keystroke, to_list, to_zipper, letter_to_char)

type Filter = List Letter
type Result = Picker Objective

type (:*+*:) l r = (l :*: r) :+: (r :*: l)

switch :: l :*+*: r -> l :*+*: r
switch (Option (l :*: r)) = Adoption (r :*: l)
switch (Adoption (r :*: l)) = Option (l :*: r)

type Texture = Filter :*+*: Maybe Result
type Search = Provision Connection :> State Texture :> Conclusion Objective :> IO

handle :: ASCII -> Search ()
handle (Control HT) = void <-- change @Texture switch
handle (Control VT) = choose_objective =<< current @Texture
handle key = update_objectives_list key =<< current @Texture

choose_objective :: Texture -> Search ()
choose_objective (Option (filter :*: Just (Turnover picker))) = failure <-- extract picker
choose_objective _ = point ()

-- TODO: this code is completely mess, we need to refactor it
-- cause I have hard time to understand its logic
update_objectives_list :: ASCII -> Texture -> Search ()
update_objectives_list key (Option (filter :*: Just picker)) =
	void <------ lift . wrap <----- zoom @Texture
		<---- perhaps @(Filter :*: Maybe Result) >>> access @(Maybe Result) >>> primary
		<---- overlook . overlook . change . constant <-- change_picker key picker
update_objectives_list key (Adoption (_ :*: filter)) = void <--------
	lift . wrap . zoom @Texture
		(perhaps @(Maybe Result :*: Filter) >>> access @(Maybe Result))
		. overlook . change @(Maybe Result) . constant
	======<< identity ======<< adapt .:.. reload_objectives_by_filter_
		<-|---- provided @Connection
		<-*---- lift . wrap .:.. zoom @Texture
			<---- perhaps @(Maybe Result :*: Filter) >>> access @Filter
			<---- overlook (change @Filter . constant <-- change_filter key filter ---* current @Filter)

-- TODO: think about caching with prefixed tree where key is a searching pattern
change_filter :: ASCII -> List Letter -> List Letter
change_filter (Letter _ letter) = item @Push letter
change_filter (Control DEL) = morph @Pop
change_filter _ = identity

change_picker :: ASCII -> Picker Objective -> Picker Objective
change_picker (Letter Lower J) = rotate @Right
change_picker (Letter Lower K) = rotate @Left
change_picker _ = identity

display :: Texture -> IO ()
display (Option (filter :*: picker)) = void
	<----- display_filter False filter
		----* resolve @(Picker Objective)
			<--- display_picker True
			<--- putStrLn --> record "No objectives found"
			<--- picker
display (Adoption (picker :*: filter)) = void
	<----- display_filter True filter
		----* resolve @(Picker Objective)
			<--- display_picker False
			<--- putStrLn <-- record "No objectives found"
			<--- picker

display_filter :: Boolean -> List Letter -> IO ()
display_filter focus filter = void <-- do
	refresh_terminal -* putStrLn ""
	putStr <--- (focus ?= True <-- focused <-- record) "Search: \ESC[7m"
	(putChar . letter_to_char <-/-- Reverse filter) -* putStrLn "" -* putStrLn "\ESC[0m"

display_picker :: Boolean -> Picker Objective -> IO ()
display_picker focus (Turnover objectives) = void <-- do
	putStrLn . record . show <-/-- view <--- sub @Left <--- view <-- sub @Rest <-- objectives
	putStrLn . (focus ?= True <-- focused <-- record) . show <-/-- get @(Convex Lens) <-- sub @Root <-- objectives
	putStrLn . record . show <-/-- view <--- sub @Right <--- view <-- sub @Rest <-- objectives

eventloop :: Search ()
eventloop = loop <----- handle
	===<< adapt . display =<< current @Texture
		---* adapt keypress

keypress :: IO ASCII
keypress = resolve @ASCII point keypress =<< run keystroke

reload_objectives_by_filter_ :: Connection -> Maybe > List Letter -> IO :. Maybe :. Picker >>> Objective
reload_objectives_by_filter_ connection (Just pattern) =
	let substring = reverse . show <---- letter_to_char <-|- pattern in
	Turnover <-|-|- to_zipper . to_list <-|- query connection "SELECT * FROM objectives WHERE title LIKE '%' || ? || '%';" (Only substring)
-- TODO: that behaviour could be a problem
reload_objectives_by_filter_ connection Nothing = point Nothing

reload_objectives_by_filter :: Connection -> List Letter -> IO :. Maybe :. Picker >>> Objective
reload_objectives_by_filter connection pattern =
	let substring = reverse . show <---- letter_to_char <-|- pattern in
	Turnover <-|-|- to_zipper . to_list <-|- query connection "SELECT * FROM objectives WHERE title LIKE '%' || ? || '%';" (Only substring)

run_search :: Connection -> IO Objective
run_search connection = do
	objectives <- reload_objectives_by_filter connection empty
	(\case { Failure obj -> obj } ) <-|- run (eventloop <~ connection <~ Option (empty :*: objectives))
