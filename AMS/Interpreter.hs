{-# OPTIONS_GHC -fno-warn-tabs #-}

module AMS.Interpreter where

import Control.Monad.State
import Control.Monad.Loops
import Data.Maybe
import Data.List
import AMS.Instruction

data InterpreterState = InterpreterState
	{
		instructions :: [Instruction],
		instructionPointer :: Int,
		stack :: [Char],
		loopStack :: [Int],
		needInput :: Bool,
		numberMode :: Bool,
		output :: String
	}

type Interpreter = State InterpreterState

interpreterState :: [Instruction] -> InterpreterState
interpreterState os = InterpreterState
	{
		instructions = os,
		instructionPointer = 0,
		stack = ['\0'],
		loopStack = [],
		needInput = False,
		numberMode = False,
		output = ""
	}

doPush :: Char -> Interpreter ()
doPush c = do
	stk <- gets stack
	modify $ \s -> s{ stack = c:stk }

doSucc :: Interpreter Int
doSucc = do
	i:stk <- gets stack
	modify $ \s -> s{ stack = (succ i):stk }
	gets instructionPointer >>= return . (+ 1)

doPred :: Interpreter Int
doPred = do
	i:stk <- gets stack
	modify $ \s -> s{ stack = (pred i):stk }
	gets instructionPointer >>= return . (+ 1)

doPop :: Interpreter Char
doPop = do
	c:stk <- gets stack
	modify $ \s -> s{ stack = if length stk == 0 then ['\0'] else stk }
	return c

doSwap :: Interpreter Int
doSwap = do
	r <- doPop
	l <- doPop
	doPush r
	doPush l
	gets instructionPointer >>= return . (+ 1)

doDup :: Interpreter Int
doDup = do
	v <- doPop
	doPush v
	doPush v
	gets instructionPointer >>= return . (+ 1)

doOver :: Interpreter Int
doOver = do
	r <- doPop
	l <- doPop
	doPush l
	doPush r
	doPush l
	gets instructionPointer >>= return . (+ 1)

doDrop :: Interpreter Int
doDrop = doPop >> gets instructionPointer >>= return . (+ 1)

doAdd :: Interpreter Int
doAdd = do
	r <- doPop
	l <- doPop
	let s = (fromEnum l) + (fromEnum r)
	doPush . toEnum $ s
	gets instructionPointer >>= return . (+ 1)

doSubtract :: Interpreter Int
doSubtract = do
	r <- doPop
	l <- doPop
	let s = (fromEnum l) - (fromEnum r)
	doPush . toEnum $ s
	gets instructionPointer >>= return . (+ 1)

doMultiply :: Interpreter Int
doMultiply = do
	r <- doPop
	l <- doPop
	let s = (fromEnum l) * (fromEnum r)
	doPush . toEnum $ s
	gets instructionPointer >>= return . (+ 1)

doDivide :: Interpreter Int
doDivide = do
	r <- doPop
	l <- doPop
	let s = (fromEnum l) * (fromEnum r)
	doPush . toEnum $ s
	gets instructionPointer >>= return . (+ 1)

doPrint :: Interpreter Int
doPrint = do
	stk <- gets stack
	out <- gets output
	let nul = fromJust . elemIndex '\0' $ stk
	let (p, r) = splitAt nul stk
	modify $ \s -> s{ stack = r, output = out ++ p }
	gets instructionPointer >>= return . (+ 1)

doPrintNum :: Interpreter Int
doPrintNum = do
	out <- gets output
	v   <- doPop
	modify $ \s -> s{ output = out ++ (show . fromEnum $ v) }
	gets instructionPointer >>= return . (+ 1)

doInChar :: Interpreter Int
doInChar = do
	modify $ \s -> s{ needInput = True, numberMode = False }
	gets instructionPointer >>= return . (+ 1)

doInNum :: Interpreter Int
doInNum = do
	modify $ \s -> s{ needInput = True, numberMode = True }
	gets instructionPointer >>= return . (+ 1)

doLoopBegin :: Interpreter Int
doLoopBegin = do
	ip   <- gets instructionPointer
	lstk <- gets loopStack
	modify $ \s -> s{ loopStack = ip:lstk }
	return $ ip + 1

doContinue :: Interpreter Int
doContinue = do
	start:lstk <- gets loopStack
	return start

doLoopEnd :: Interpreter Int
doLoopEnd = do
	start:lstk <- gets loopStack
	modify $ \s -> s{ loopStack = lstk }
	return start

doBreak :: Interpreter Int
doBreak = do
	start:lstk <- gets loopStack
	is <- gets instructions
	ip <- gets instructionPointer
	modify $ \s -> s{ loopStack = lstk }
	let end = fromJust . elemIndex LoopEnd $ (drop ip is)
	return $ ip + end + 1

doCompareEQ :: Interpreter Int
doCompareEQ = do
	r <- doPop
	l <- doPop
	doPush l
	doPush r
	ip <- gets instructionPointer
	return $ if l == r then ip + 1 else ip + 2

doCompareGT :: Interpreter Int
doCompareGT = do
	r <- doPop
	l <- doPop
	doPush l
	doPush r
	ip <- gets instructionPointer
	return $ if l > r then ip + 1 else ip + 2

execute :: Maybe Char -> Interpreter Bool
execute input = do
	ni <- gets needInput
	when ni $ doPush (fromJust input) >> return ()
	modify $ \s -> s{ needInput = False }
	whileM_ more $ do
		is <- gets instructions
		ip <- gets instructionPointer
		np <- execute' $ is !! ip
		modify $ \s -> s{ instructionPointer = np }
	gets needInput >>= return
	where
		more = do
			is <- gets instructions
			ip <- gets instructionPointer
			ni <- gets needInput
			return (ip < length is && not ni)
		execute' i =
			case i of
				Succ        -> doSucc
				Pred        -> doPred
				Swap        -> doSwap
				Dup         -> doDup
				Over        -> doOver
				Drop        -> doDrop
				Add         -> doAdd
				Subtract    -> doSubtract
				Multiply    -> doMultiply
				Divide      -> doDivide
				Print       -> doPrint
				PrintNum    -> doPrintNum
				InChar      -> doInChar
				InNum       -> doInNum
				LoopBegin   -> doLoopBegin
				Continue    -> doContinue
				LoopEnd     -> doLoopEnd
				Break       -> doBreak
				CompareEQ   -> doCompareEQ
				CompareGT   -> doCompareGT
				NOP         -> gets instructionPointer >>= return . (+ 1)
