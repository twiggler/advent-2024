module Instruction
  ( adv,
    bxl,
    bst,
    jnz,
    out,
    bxc,
    bdv,
    cdv,
  )
where

import Data.Bits (xor)
import Data.Finite (getFinite)
import Machine (MonadMachineIO, MonadMachineRegisters (..), Word3, write, Address)

adv :: (MonadMachineRegisters m) => Integer -> m ()
adv combo = do
  regA <- getRegisterA
  let result = regA `div` (2 ^ combo)
  setRegisterA result

bxl :: (MonadMachineRegisters m) => Word3 -> m ()
bxl literal = do
  regB <- getRegisterB
  let result = regB `xor` getFinite literal
  setRegisterB result

bst :: (MonadMachineRegisters m) => Integer -> m ()
bst combo = setRegisterB (combo `rem` 8)

jnz :: (MonadMachineRegisters m) => Word3 -> m (Maybe Address)
jnz literal = do
  regA <- getRegisterA
  if regA == 0
    then return Nothing
    else
      return . Just $ literal

bxc :: (MonadMachineRegisters m) => m ()
bxc = do
  regB <- getRegisterB
  regC <- getRegisterC
  setRegisterB (regB `xor` regC)

out :: (MonadMachineIO m) => Integer -> m ()
out combo = write (combo `rem` 8)

bdv :: (MonadMachineRegisters m) => Integer -> m ()
bdv combo = do
  regA <- getRegisterA
  let result = regA `div` (2 ^ combo)
  setRegisterB result

cdv :: (MonadMachineRegisters m) => Integer -> m ()
cdv combo = do
  regA <- getRegisterA
  let result = regA `div` (2 ^ combo)
  setRegisterC result
