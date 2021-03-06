module Control.TUI where

import "base" Data.Bool (Bool (False))
import "base" Data.Semigroup ((<>))
import "base" System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, hFlush, putStr, putStrLn, stdin, stdout)

prepare_terminal = do
	hSetBuffering stdin NoBuffering
	hSetEcho stdin False
	hide_cursor

refresh_terminal = do
	clear_terminal
	up_cursor

prompt line = do
	hSetBuffering stdin NoBuffering
	hFlush stdout
	putStr line

clear_terminal = putStr "\ESC[2J"
up_cursor = putStr "\ESC[100A"
hide_cursor = putStr "\ESC[?25l"

newline = putStr "\n"

negative text = "\ESC[7m" <> text <> "\ESC[0m"
underlined text = "\ESC[4m" <> text <> "\ESC[0m"
bold text = "\ESC[1m" <> text <> "\ESC[0m"
heading text = "\n" <> text <> "\n"

line text = "    " <> text
focused text = " => " <> bold text
record text = "    " <> text
