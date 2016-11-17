{-# OPTIONS_GHC -fno-warn-tabs #-}

module AMS.Note where

import Data.List
import Data.Maybe
import qualified Text.ParserCombinators.Parsec as P

-- A simple musical note value
-- Derive Show to allow formatted printing with putStrLn, show, etc.
-- Derive Enum to alllow to toEnum/fromEnum conversions
data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B deriving (Show, Enum)

-- Parse a single Note value
note :: P.Parser Note
note = let ns = "CcDdEFfGgAaB" in P.oneOf ns >>= \n ->
	-- The element index always exists, lift to Note and then to Parser
	return . toEnum . fromJust . elemIndex n $ ns

-- A complete program is a stream of Notes
program :: P.Parser [Note]
program = P.many (note <* P.spaces)