module Control.Pandora.Event where

import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import "base" Data.Int (Int)
import "base" Data.String (String)

type Event = String :*: String :*: String :*: String
