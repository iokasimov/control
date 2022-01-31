{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Pandora.Widgets.Search where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.List (reverse)
import "base" Text.Show (show)
import "base" System.IO (putStrLn, putStr, putChar)
import "sqlite-simple" Database.SQLite.Simple (Connection, Only (Only), query)

import Control.Pandora.Entity.Objective (Objective)
import Control.Pandora.SQLite ()
import Control.Pandora.Widgets.Components.Picker (Picker)
import Control.Pandora.TUI (refresh_terminal, focused, record)
import Control.Pandora.Utils (keystroke, to_list, to_zipper, letter_to_char)

type Texture = (List Letter :*: Maybe # Picker Objective) :+: Flip (:*:) # Maybe (Picker Objective) # List Letter

type Search = Provision Connection :> State Texture :> Conclusion Objective :> IO

handle :: ASCII -> Search ()
handle (Control HT) = void . adapt . modify @State @Texture ! \case
	Option picker -> Adoption <-- Flip picker
	Adoption (Flip searcher) -> Option searcher
handle (Control VT) = choose_objective ===<< adapt <-- get @State
handle key = update_objectives_list key ===<< adapt <-- get @State

choose_objective :: Texture -> Search ()
choose_objective (Option (filter :*: Just (Turnover picker))) = failure <-- extract picker
choose_objective _ = point ()

update_objectives_list :: ASCII -> Texture -> Search ()
update_objectives_list key (Option (filter :*: picker)) = void . adapt . set @State @Texture . Option ! filter :*: (change_picker key <-|- picker)
update_objectives_list key (Adoption (Flip (filter :*: _))) = let new = change_filter key filter in
	void . adapt . set @State @Texture . Adoption . Flip . (new :*:) ===<< identity ===<< (adapt . (reload_objectives_by_filter % new)) <-|- provided @Connection

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
display (Option (filter :*: picker)) = void ! do
	(resolve @(Picker Objective)
		<--- display_picker True
		<--- putStrLn --> record "No objectives found"
		<--- picker)
	.-*- display_filter False filter
display (Adoption (Flip (filter :*: picker))) = void !
	(resolve @(Picker Objective)
		<--- display_picker False
		<--- putStrLn <-- record "No objectives found"
		<--- picker)
	.-*- display_filter True filter

display_filter :: Boolean -> List Letter -> IO ()
display_filter focus filter = void ! do
	putStrLn "" .-*- refresh_terminal
	putStr ! (focus ? focused ! record) "Search: \ESC[7m"
	putStrLn "\ESC[0m" .-*- putStrLn "" .-*- (putChar . letter_to_char <<-- Reverse filter)

display_picker :: Boolean -> Picker Objective -> IO ()
display_picker focus (Turnover objectives) = void ! do
	putStrLn . record . show <<--- get @(Convex Lens) <-- sub @Left <-- objectives
	putStrLn . (focus ? focused ! record) . show <<--- get @(Convex Lens) <-- sub @Root <-- objectives
	putStrLn . record . show <<--- get @(Convex Lens) <-- sub @Right <-- objectives

eventloop :: Search ()
eventloop = forever_ ! handle ===<< adapt keypress .-*- (adapt . display ===<< adapt <-- get @State)

keypress :: IO ASCII
keypress = resolve @ASCII point keypress =<< run keystroke

reload_objectives_by_filter :: Connection -> List Letter -> IO :. Maybe :. Picker := Objective
reload_objectives_by_filter connection pattern = 
	let substring = reverse . show ! letter_to_char <-|- pattern in
	Turnover <-|-|- to_zipper . to_list <-|- query connection "SELECT * FROM objectives WHERE title LIKE '%' || ? || '%';" (Only substring)

run_search :: Connection -> IO Objective
run_search connection = do
	objectives <- reload_objectives_by_filter connection empty
	(\case { Failure obj -> obj } ) <-|- run (eventloop ! connection ! Option (empty :*: objectives))
