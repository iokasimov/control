{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.Widgets.Components.Picker where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

type Picker = Turnover (Tape List)
