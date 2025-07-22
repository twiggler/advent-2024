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
import Data.Finite.Integral (finite)

adv :: (MonadMachineRegisters m) => Integer -> m ()
adv combo = do
  regA <- getRegisterA
  setRegisterA (regA `div` (2 ^ combo))

bxl :: (MonadMachineRegisters m) => Word3 -> m ()
bxl literal = do
  regB <- getRegisterB
  setRegisterB (regB `xor` getFinite literal)

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
out combo = write (finite $ combo `rem` 8)

bdv :: (MonadMachineRegisters m) => Integer -> m ()
bdv combo = do
  regA <- getRegisterA
  setRegisterB (regA `div` (2 ^ combo))

cdv :: (MonadMachineRegisters m) => Integer -> m ()
cdv combo = do
  regA <- getRegisterA
  setRegisterC (regA `div` (2 ^ combo))
