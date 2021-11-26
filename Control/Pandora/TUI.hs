module Control.Pandora.TUI where

import "base" Data.Bool (Bool (False))
import "base" System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, putStr, putStrLn, stdin)

prepare_terminal = do
	hSetBuffering stdin NoBuffering
	hSetEcho stdin False
	hide_cursor

clear_terminal = putStr "\ESC[2J"
up_cursor = putStr "\ESC[100A"
hide_cursor = putStr "\ESC[?25l"

newline = putStr "\n"
