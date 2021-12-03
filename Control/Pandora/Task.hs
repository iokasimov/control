module Control.Pandora.Task where

import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import "base" Data.Int (Int)
import "base" Data.String (String)
import "base" Text.Show (Show)
import "sqlite-simple" Database.SQLite.Simple (SQLData (SQLInteger), FromRow (fromRow), field)
import "sqlite-simple" Database.SQLite.Simple.Internal (Field (Field))

type Task = Int :*: Status :*: Int :*: String :*: String :*: String

data Status = TODO | DONE | GONE | LATE deriving Show

