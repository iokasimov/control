module Control.Entity.Resource where

import "base" Data.List (intersperse)
import "pandora" Pandora.Pattern.Functor.Covariant ((<-|-|-))
import "pandora" Pandora.Paradigm.Structure.Some.List (List)
import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import Control.Entity.ID (ID)
import Control.Entity.Objective (Objective)
import Control.Entity.Status (Status)

type Resource = Status :*: String :*: String :*: String

instance Show Resource where
	show (_ :*: title :*: link :*: tags) =
		title <> "\n" <> link <> "\n" <> tags
