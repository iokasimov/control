module Control.Pandora.Entity.Event where

import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

type Event = String :*: String :*: String :*: String

instance Show Event where
	show (title :*: start :*: stop :*: total) =
		"{" <> start <> " - " <> stop <> " => " <> total <> "} " <> title
