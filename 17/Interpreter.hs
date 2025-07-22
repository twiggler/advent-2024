{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}

module Interpreter
  ( Interpreter,
    mkInterpreter,
    run,
  )
where

import Control.Comonad (extract)
import Control.Comonad.Store (Store)
import Control.Comonad.Store qualified as S (ComonadStore (seeks), seek, store)
import Control.Monad (when)
import Control.Monad.State.Lazy (State, evalState)
import Control.Monad.State.Lazy qualified as St (gets, modify')
import Control.Monad.Writer.Lazy (WriterT, execWriterT)
import Control.Monad.Writer.Lazy qualified as W (MonadWriter (tell))
import Data.Finite (getFinite)
import Data.Maybe (isJust)
import Data.Vector ((!?))
import Data.Vector qualified as V (fromList)
import Instruction (adv, bdv, bst, bxc, bxl, cdv, jnz, out)
import Machine

type InterpreterM = WriterT [Word3] (State Interpreter)

data Interpreter = Interpreter
  { registerA :: Integer,
    registerB :: Integer,
    registerC :: Integer,
    program :: Store Int (Maybe SomeInstruction)
  }

newtype MonadInterpreter a = MonadInterpreter {runMonadInterpreter :: InterpreterM a}
  deriving (Functor, Applicative, Monad) via InterpreterM

instance MonadMachineRegisters MonadInterpreter where
  getRegisterA = MonadInterpreter $ St.gets registerA
  getRegisterB = MonadInterpreter $ St.gets registerB
  getRegisterC = MonadInterpreter $ St.gets registerC
  setRegisterA x = MonadInterpreter $ St.modify' (\m -> m {registerA = x})
  setRegisterB x = MonadInterpreter $ St.modify' (\m -> m {registerB = x})
  setRegisterC x = MonadInterpreter $ St.modify' (\m -> m {registerC = x})

instance MonadMachineIO MonadInterpreter where
  write x = MonadInterpreter $ W.tell [x]

peekInstruction :: MonadInterpreter (Maybe SomeInstruction)
peekInstruction = MonadInterpreter $ St.gets (extract . program)

bumpIp :: MonadInterpreter ()
bumpIp = MonadInterpreter $ St.modify' (\m -> m {program = S.seeks (+ 1) (program m)})

setIp :: Address -> MonadInterpreter ()
setIp ip = MonadInterpreter $ St.modify' (\m -> m {program = S.seek (toInt ip) (program m)})
  where
    toInt = fromIntegral . getFinite

evalOperand :: Operand a -> MonadInterpreter a
evalOperand (Combo RegA) = getRegisterA
evalOperand (Combo RegB) = getRegisterB
evalOperand (Combo RegC) = getRegisterC
evalOperand (Combo (Lit l)) = return $ toInteger l
evalOperand (Literal l) = return l
evalOperand (None _) = return ()

mkInterpreter :: MachineConfig -> Interpreter
mkInterpreter (MachineConfig regA regB regC instructions') =
  let program' = V.fromList instructions'
      programStore = S.store (program' !?) 0
   in Interpreter regA regB regC programStore

step :: MonadInterpreter Bool
step = do
  currentInstruction <- peekInstruction

  case currentInstruction of
    Just (SomeInstruction (Adv op)) -> evalOperand op >>= adv >> bumpIp
    Just (SomeInstruction (Bxl op)) -> evalOperand op >>= bxl >> bumpIp
    Just (SomeInstruction (Bst op)) -> evalOperand op >>= bst >> bumpIp
    Just (SomeInstruction (Jnz op)) -> evalOperand op >>= jnz >>= adjustIp
    Just (SomeInstruction (Bxc _)) -> bxc >> bumpIp
    Just (SomeInstruction (Out op)) -> evalOperand op >>= out >> bumpIp
    Just (SomeInstruction (Bdv op)) -> evalOperand op >>= bdv >> bumpIp
    Just (SomeInstruction (Cdv op)) -> evalOperand op >>= cdv >> bumpIp
    Nothing -> return ()

  return (isJust currentInstruction)
  where
    adjustIp = maybe bumpIp setIp

run :: Interpreter -> [Word3]
run = evalState (execWriterT (runMonadInterpreter loop))
  where
    loop = step >>= \result -> when result loop -- Thunk buildup, but the output should be streamable
