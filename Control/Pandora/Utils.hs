{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Pandora.Utils where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Char (Char, ord)
import "base" Data.List ((++))
import "base" Data.String (String)
import "base" System.IO (getChar)
import "base" Text.Show (Show (show))

instance Semigroup String where
	(+) = (++)

deriving instance Show a => Show (Maybe a)

deriving instance Show Control
deriving instance Show Case
deriving instance Show Letter
deriving instance Show Number
deriving instance Show Bracket
deriving instance Show Sign
deriving instance Show Quote
deriving instance Show Slash
deriving instance Show Position
deriving instance Show ASCII

instance Show (List Char) where
	show (TT (Just (Construct x xs))) = x : show (TT @Covariant @Covariant xs)
	show (TT Nothing) = ""

list_to_list :: List a -> [a] -> List a
list_to_list ys (x : xs) = list_to_list <-- item @Push x ys <-- xs
list_to_list ys [] = ys

to_list :: [a] -> List a
to_list = list_to_list empty

to_zipper :: List a -> Maybe :. Tape List >>> a
to_zipper = run . into @(Tape List)

keystroke :: Maybe :> IO >>> ASCII
keystroke = unite <---- castASCII <-|- getChar

letter_to_char :: Letter -> Char
letter_to_char A = 'A'
letter_to_char B = 'B'
letter_to_char C = 'C'
letter_to_char D = 'D'
letter_to_char E = 'E'
letter_to_char F = 'F'
letter_to_char G = 'G'
letter_to_char H = 'H'
letter_to_char I = 'I'
letter_to_char J = 'J'
letter_to_char K = 'K'
letter_to_char L = 'L'
letter_to_char M = 'M'
letter_to_char N = 'N'
letter_to_char O = 'O'
letter_to_char P = 'P'
letter_to_char R = 'R'
letter_to_char S = 'S'
letter_to_char T = 'T'
letter_to_char U = 'U'
letter_to_char V = 'V'
letter_to_char W = 'W'
letter_to_char X = 'X'
letter_to_char Y = 'Y'
letter_to_char Z = 'Z'

castASCII :: Char -> Maybe ASCII
castASCII c = case ord c of
	0 -> Just <-- Control NUL
	1 -> Just <-- Control SOH
	2 -> Just <-- Control STX
	3 -> Just <-- Control ETX
	4 -> Just <-- Control EOT
	5 -> Just <-- Control ENQ
	6 -> Just <-- Control ACK
	7 -> Just <-- Control BEL
	8 -> Just <-- Control BS
	9 -> Just <-- Control HT
	10 -> Just <-- Control VT
	11 -> Just <-- Control FF
	12 -> Just <-- Control CR
	13 -> Just <-- Control SO
	14 -> Just <-- Control SI
	15 -> Just <-- Control DLE
	16 -> Just <-- Control DC1
	17 -> Just <-- Control DC2
	18 -> Just <-- Control DC3
	19 -> Just <-- Control DC4
	20 -> Just <-- Control NAK
	21 -> Just <-- Control SYN
	22 -> Just <-- Control ETB
	23 -> Just <-- Control CAN
	24 -> Just <-- Control EM
	25 -> Just <-- Control SUB
	26 -> Just <-- Control ETB
	27 -> Just <-- Control ESC
	28 -> Just <-- Control FS
	29 -> Just <-- Control GS
	30 -> Just <-- Control RS
	31 -> Just <-- Control US
	32 -> Just <-- Control SP
	33 -> Just <-- Sign Exclamation
	34 -> Just . Sign <-- Quote Double
	35 -> Just <-- Sign Hash
	36 -> Just <-- Sign Dollar
	37 -> Just <-- Sign Percent
	38 -> Just <-- Sign Ampersand
	39 -> Just <-- Sign Apostrophe
	40 -> Just . Sign <-- Bracket Opened Round
	41 -> Just . Sign <-- Bracket Closed Round
	42 -> Just <-- Sign Asterisk
	43 -> Just <-- Sign Plus
	44 -> Just <-- Sign Comma
	45 -> Just <-- Sign Minus
	46 -> Just <-- Sign Period
	47 -> Just . Sign <-- Slash Forward
	48 -> Just <-- Number N0
	49 -> Just <-- Number N1
	50 -> Just <-- Number N2
	51 -> Just <-- Number N3
	52 -> Just <-- Number N4
	53 -> Just <-- Number N5
	54 -> Just <-- Number N6
	55 -> Just <-- Number N7
	56 -> Just <-- Number N8
	57 -> Just <-- Number N9
	58 -> Just <-- Sign Colon
	59 -> Just <-- Sign Semicolon
	60 -> Just . Sign <-- Bracket Opened Angle
	61 -> Just <-- Sign Equality
	62 -> Just . Sign <-- Bracket Closed Angle
	63 -> Just <-- Sign Question
	64 -> Just <-- Sign At
	65 -> Just <-- Letter Upper A
	66 -> Just <-- Letter Upper B
	67 -> Just <-- Letter Upper C
	68 -> Just <-- Letter Upper D
	69 -> Just <-- Letter Upper E
	70 -> Just <-- Letter Upper F
	71 -> Just <-- Letter Upper G
	72 -> Just <-- Letter Upper H
	73 -> Just <-- Letter Upper I
	74 -> Just <-- Letter Upper J
	75 -> Just <-- Letter Upper K
	76 -> Just <-- Letter Upper L
	77 -> Just <-- Letter Upper M
	78 -> Just <-- Letter Upper N
	79 -> Just <-- Letter Upper O
	80 -> Just <-- Letter Upper P
	81 -> Just <-- Letter Upper Q
	82 -> Just <-- Letter Upper R
	83 -> Just <-- Letter Upper S
	84 -> Just <-- Letter Upper T
	85 -> Just <-- Letter Upper U
	86 -> Just <-- Letter Upper V
	87 -> Just <-- Letter Upper W
	88 -> Just <-- Letter Upper X
	89 -> Just <-- Letter Upper Y
	90 -> Just <-- Letter Upper Z
	91 -> Just . Sign <-- Bracket Opened Square
	92 -> Just . Sign <-- Slash Back
	93 -> Just . Sign <-- Bracket Closed Square
	94 -> Just <-- Sign Caret
	95 -> Just <-- Sign Underscore
	96 -> Just <-- Sign Accent
	97 -> Just <-- Letter Lower A
	98 -> Just <-- Letter Lower B
	99 -> Just <-- Letter Lower C
	100 -> Just <-- Letter Lower D
	101 -> Just <-- Letter Lower E
	102 -> Just <-- Letter Lower F
	103 -> Just <-- Letter Lower G
	104 -> Just <-- Letter Lower H
	105 -> Just <-- Letter Lower I
	106 -> Just <-- Letter Lower J
	107 -> Just <-- Letter Lower K
	108 -> Just <-- Letter Lower L
	109 -> Just <-- Letter Lower M
	110 -> Just <-- Letter Lower N
	111 -> Just <-- Letter Lower O
	112 -> Just <-- Letter Lower P
	113 -> Just <-- Letter Lower Q
	114 -> Just <-- Letter Lower R
	115 -> Just <-- Letter Lower S
	116 -> Just <-- Letter Lower T
	117 -> Just <-- Letter Lower U
	118 -> Just <-- Letter Lower V
	119 -> Just <-- Letter Lower W
	120 -> Just <-- Letter Lower X
	121 -> Just <-- Letter Lower Y
	122 -> Just <-- Letter Lower Z
	123 -> Just . Sign <-- Bracket Opened Curly
	124 -> Just <-- Sign Bar
	125 -> Just . Sign <-- Bracket Closed Curly
	126 -> Just <-- Sign Tilde
	127 -> Just <-- Control DEL
	_ -> Nothing
