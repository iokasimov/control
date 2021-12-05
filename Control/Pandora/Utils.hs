{-# LANGUAGE NoImplicitPrelude #-}

module Control.Pandora.Utils where

import "pandora" Pandora.Core
import "pandora" Pandora.Paradigm
import "pandora" Pandora.Pattern
import "pandora-io" Pandora.IO

import "base" Data.Char (Char, ord)
import "base" Data.List ((++))
import "base" Data.String (String)

instance Semigroup String where
	(+) = (++)

list_to_list :: List a -> [a] -> List a
list_to_list ys (x : xs) = list_to_list # item @Push x ys # xs
list_to_list ys [] = ys

castASCII :: Char -> Maybe ASCII
castASCII c = case ord c of
	0 -> Just $ Control NUL
	1 -> Just $ Control SOH
	2 -> Just $ Control STX
	3 -> Just $ Control ETX
	4 -> Just $ Control EOT
	5 -> Just $ Control ENQ
	6 -> Just $ Control ACK
	7 -> Just $ Control BEL
	8 -> Just $ Control BS
	9 -> Just $ Control HT
	10 -> Just $ Control VT
	11 -> Just $ Control FF
	12 -> Just $ Control CR
	13 -> Just $ Control SO
	14 -> Just $ Control SI
	15 -> Just $ Control DLE
	16 -> Just $ Control DC1
	17 -> Just $ Control DC2
	18 -> Just $ Control DC3
	19 -> Just $ Control DC4
	20 -> Just $ Control NAK
	21 -> Just $ Control SYN
	22 -> Just $ Control ETB
	23 -> Just $ Control CAN
	24 -> Just $ Control EM
	25 -> Just $ Control SUB
	26 -> Just $ Control ETB
	27 -> Just $ Control ESC
	28 -> Just $ Control FS
	29 -> Just $ Control GS
	30 -> Just $ Control RS
	31 -> Just $ Control US
	32 -> Just $ Control SP
	33 -> Just $ Sign Exclamation
	34 -> Just $ Sign $ Quote Double
	35 -> Just $ Sign Hash
	36 -> Just $ Sign Dollar
	37 -> Just $ Sign Percent
	38 -> Just $ Sign Ampersand
	39 -> Just $ Sign Apostrophe
	40 -> Just $ Sign $ Bracket Opened Round
	41 -> Just $ Sign $ Bracket Closed Round
	42 -> Just $ Sign Asterisk
	43 -> Just $ Sign Plus
	44 -> Just $ Sign Comma
	45 -> Just $ Sign Minus
	46 -> Just $ Sign Period
	47 -> Just $ Sign $ Slash Forward
	48 -> Just $ Number N0
	49 -> Just $ Number N1
	50 -> Just $ Number N2
	51 -> Just $ Number N3
	52 -> Just $ Number N4
	53 -> Just $ Number N5
	54 -> Just $ Number N6
	55 -> Just $ Number N7
	56 -> Just $ Number N8
	57 -> Just $ Number N9
	58 -> Just $ Sign Colon
	59 -> Just $ Sign Semicolon
	60 -> Just $ Sign $ Bracket Opened Angle
	61 -> Just $ Sign Equality
	62 -> Just $ Sign $ Bracket Closed Angle
	63 -> Just $ Sign Question
	64 -> Just $ Sign At
	65 -> Just $ Leter Upper A
	66 -> Just $ Leter Upper B
	67 -> Just $ Leter Upper C
	68 -> Just $ Leter Upper D
	69 -> Just $ Leter Upper E
	70 -> Just $ Leter Upper F
	71 -> Just $ Leter Upper G
	72 -> Just $ Leter Upper H
	73 -> Just $ Leter Upper I
	74 -> Just $ Leter Upper J
	75 -> Just $ Leter Upper K
	76 -> Just $ Leter Upper L
	77 -> Just $ Leter Upper M
	78 -> Just $ Leter Upper N
	79 -> Just $ Leter Upper O
	80 -> Just $ Leter Upper P
	81 -> Just $ Leter Upper Q
	82 -> Just $ Leter Upper R
	83 -> Just $ Leter Upper S
	84 -> Just $ Leter Upper T
	85 -> Just $ Leter Upper U
	86 -> Just $ Leter Upper V
	87 -> Just $ Leter Upper W
	88 -> Just $ Leter Upper X
	89 -> Just $ Leter Upper Y
	90 -> Just $ Leter Upper Z
	91 -> Just $ Sign $ Bracket Opened Square
	92 -> Just $ Sign $ Slash Back
	93 -> Just $ Sign $ Bracket Closed Square
	94 -> Just $ Sign Caret
	95 -> Just $ Sign Underscore
	96 -> Just $ Sign Accent
	97 -> Just $ Leter Lower A
	98 -> Just $ Leter Lower B
	99 -> Just $ Leter Lower C
	100 -> Just $ Leter Lower D
	101 -> Just $ Leter Lower E
	102 -> Just $ Leter Lower F
	103 -> Just $ Leter Lower G
	104 -> Just $ Leter Lower H
	105 -> Just $ Leter Lower I
	106 -> Just $ Leter Lower J
	107 -> Just $ Leter Lower K
	108 -> Just $ Leter Lower L
	109 -> Just $ Leter Lower M
	110 -> Just $ Leter Lower N
	111 -> Just $ Leter Lower O
	112 -> Just $ Leter Lower P
	113 -> Just $ Leter Lower Q
	114 -> Just $ Leter Lower R
	115 -> Just $ Leter Lower S
	116 -> Just $ Leter Lower T
	117 -> Just $ Leter Lower U
	118 -> Just $ Leter Lower V
	119 -> Just $ Leter Lower W
	120 -> Just $ Leter Lower X
	121 -> Just $ Leter Lower Y
	122 -> Just $ Leter Lower Z
	123 -> Just $ Sign $ Bracket Opened Curly
	124 -> Just $ Sign Bar
	125 -> Just $ Sign $ Bracket Closed Curly
	124 -> Just $ Sign Tilde
	_ -> Nothing
