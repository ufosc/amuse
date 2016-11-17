{-# OPTIONS_GHC -fno-warn-tabs #-}

import AMS.Note
import AMS.Instruction
import AMS.Interpreter
import qualified Codec.Midi as M

import Control.Monad.State
import System.Environment


main :: IO ()
main = do
	args <- getArgs
	Right mf <- M.importFile $ args !! 0
	let tb = M.timeDiv mf
	let md = concat . M.tracks $ mf
	let os = filter (M.isNoteOn . snd) md
	let ns = filter ((> 0) . M.velocity . snd) os
	let cs = map (toEnum . flip mod 12 . M.key . snd) ns :: [Note]

	let is = toInstructions cs
	run Nothing (interpreterState is)
	where run input ls = do
		let (need, s) = runState (execute input) ls
		let out = output s
		when (length out > 0) $ putStrLn . output $ s
		when need $ do
			str <- getLine
			let c = if numberMode s then toEnum . read $ str else str !! 0
			run (Just c) s{ output = "" }
