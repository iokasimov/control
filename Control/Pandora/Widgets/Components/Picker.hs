{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Pandora.Widgets.Components.Picker where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

type Picker = Turnover (Tape List)

move :: forall direction . Morphed # Rotate direction # Picker # Picker => Picker ~> Picker
move z = rotate @direction z
