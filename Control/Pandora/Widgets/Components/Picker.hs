{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Pandora.Widgets.Components.Picker where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

type Picker = Tape List

move :: forall direction . Morphed # Rotate direction # Picker # Maybe <::> Picker => Picker ~> Picker
move z = resolve @(Picker _) identity z # run (rotate @direction z)
