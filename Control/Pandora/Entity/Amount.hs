module Control.Pandora.Entity.Amount where

import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

type Amount = String :*: String :*: Int

instance Show Amount where
	show (title :*: hours :*: percentage) =
		"{" <> hours <> " => " <> show_percentage percentage <> "} " <> title

show_percentage :: Int -> String
show_percentage p = (if p < 10 then " " <> show p else show p) <> "%"
