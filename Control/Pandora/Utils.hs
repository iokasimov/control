module Control.Pandora.Utils where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

list_to_list :: [a] -> List a -> List a
list_to_list (x : xs) ys = list_to_list xs # item @Push x ys
list_to_list [] ys = ys
