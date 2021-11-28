{-# LANGUAGE NoImplicitPrelude #-}

module Control.Pandora.Utils where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.List ((++))
import "base" Data.String (String)

instance Semigroup String where
	(+) = (++)

list_to_list :: List a -> [a] -> List a
list_to_list ys (x : xs) = list_to_list # item @Push x ys # xs
list_to_list ys [] = ys
