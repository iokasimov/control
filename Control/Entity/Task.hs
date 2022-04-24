module Control.Entity.Task where

import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import Control.Entity.ID (ID)
import Control.Entity.Objective (Objective)

type Task = ID () :*: Status :*: Int :*: ID Objective :*: String :*: String :*: String

instance Show Task where
	show (_ :*: status :*: mode :*: oid :*: title :*: start :*: stop) =
		"[" <> show status <> "] " <> "{" <> start <> " - " <> stop <> "} " <> title

data Status = TODO | DONE | GONE | LATE deriving Show
