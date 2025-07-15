{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Machine
  ( Word3,
    Address,
    Combo (..),
    Operand (..),
    Instruction (..),
    SomeInstruction (..),
    MonadMachineRegisters (..),
    MonadMachineIO (..),
    MachineConfig (..),
  )
where

import Data.Finite (Finite)

type Word3 = Finite 8
type Address = Word3

data Combo = RegA | RegB | RegC | Lit (Finite 4)

data Operand a where
  Literal :: Word3 -> Operand Word3
  Combo :: Combo -> Operand Integer
  None :: Operand ()

data Instruction a where
  Adv :: Operand Integer -> Instruction (Operand Integer)
  Bxl :: Operand Word3 -> Instruction (Operand Word3)
  Bst :: Operand Integer -> Instruction (Operand Integer)
  Jnz :: Operand Word3 -> Instruction (Operand Word3)
  Bxc :: Operand () -> Instruction (Operand ())
  Out :: Operand Integer -> Instruction (Operand Integer)
  Bdv :: Operand Integer -> Instruction (Operand Integer)
  Cdv :: Operand Integer -> Instruction (Operand Integer)

data SomeInstruction where
  SomeInstruction :: Instruction a -> SomeInstruction

data MachineConfig = MachineConfig
  { initialA :: Integer
  , initialB :: Integer
  , initialC :: Integer
  , instructions :: [SomeInstruction]
  }

class (Monad m) => MonadMachineRegisters m where
  getRegisterA :: m Integer
  getRegisterB :: m Integer
  getRegisterC :: m Integer
  setRegisterA :: Integer -> m ()
  setRegisterB :: Integer -> m ()
  setRegisterC :: Integer -> m ()

class (Monad m) => MonadMachineIO m where
  write :: Integer -> m ()
