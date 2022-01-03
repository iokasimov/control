{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Pandora.Widgets.Components.Picker where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

type Picker = Tape List

move :: forall direction a . Morphed # Rotate direction # Tape List # Maybe <::> Tape List => Tape List a -> Tape List a
move z = resolve @(Tape List a) identity z # run (rotate @direction z)
