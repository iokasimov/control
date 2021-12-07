module Control.Pandora.Entity.Task where

import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import "base" Data.Int (Int)
import "base" Data.String (String)
import "base" Text.Show (Show)

type Task = Int :*: Status :*: Int :*: String :*: String :*: String

data Status = TODO | DONE | GONE | LATE deriving Show

