{-# OPTIONS_GHC -fno-warn-tabs #-}

module AMS.Instruction where

import AMS.Note

-- All possible instructions represented by note values
-- Instructions on the same line are Normal/Alternate pairs
-- Alternate instructions represented by an A Sharp prefix note
data Instruction
	= Succ      | Pred
	| Swap
	| Dup       | Over
	| Drop
	| Add       | Subtract
	| Multiply  | Divide
	-- Print an ASCII string to a null character, or a raw numeric value
	| Print     | PrintNum
	-- Input an ASCII character, or parse a number
	| InChar    | InNum
	| LoopBegin | Continue
	| LoopEnd   | Break
	| CompareEQ | CompareGT
	| Alternate | NOP
	deriving (Show, Eq)

-- Instructions represented by specific note values
opcodes :: [[Instruction]]
opcodes =
	[
		-- Non-Alternate instructions
		[
			Succ, InChar, Swap, LoopBegin, Dup, Drop, LoopEnd, Add, CompareEQ, Multiply, Alternate, Print
		],
		-- Alternate instructions
		[
			Pred, InNum, NOP, Continue, Over, NOP, Break, Subtract, CompareGT, Divide, NOP, PrintNum
		]
	]

-- Parse some Notes to decode an instruction
-- Take in a set of Note, and return the Operation and remaining Notes
-- Instructions are variable length thanks to the Alternate encoding
toInstruction :: [Note] -> (Instruction, [Note])
toInstruction (t1:ts) = case t1 of
	-- This is an alternate encoding, parse the next Note in the list
	As -> toInstruction' True  (ts)
	-- Normal encoding, parse this Note again
	otherwise        -> toInstruction' False (t1:ts)
	where toInstruction' alt (n:ts) = let
			ne = fromEnum n
			in (opcodes !! (fromEnum alt) !! ne, ts)

toInstructions :: [Note] -> [Instruction]
toInstructions [] = []
toInstructions ts = let (o, tr) = toInstruction ts in o:(toInstructions tr)