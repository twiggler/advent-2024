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
    ToWords (..)
  )
where

import Data.Finite (Finite)
import Data.Finite.Integral (finite)

type Word3 = Finite 8
type Address = Word3

data Combo = RegA | RegB | RegC | Lit (Finite 4)

data Operand a where
  Literal :: Word3 -> Operand Word3
  Combo :: Combo -> Operand Integer
  None :: Word3 -> Operand ()

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

class ToWords a where
  toWords :: a -> [Word3]

instance ToWords SomeInstruction where
  toWords (SomeInstruction (Adv op)) = 0 : toWords op
  toWords (SomeInstruction (Bxl op)) = 1 : toWords op
  toWords (SomeInstruction (Bst op)) = 2 : toWords op
  toWords (SomeInstruction (Jnz op)) = 3 : toWords op
  toWords (SomeInstruction (Bxc op)) = 4 : toWords op
  toWords (SomeInstruction (Out op)) = 5 : toWords op
  toWords (SomeInstruction (Bdv op)) = 6 : toWords op
  toWords (SomeInstruction (Cdv op)) = 7 : toWords op

instance ToWords (Operand a) where
  toWords (Literal w) = [w]
  toWords (Combo (Lit l)) = [finite $ toInteger l]
  toWords (Combo RegA) = [4]
  toWords (Combo RegB) = [5]
  toWords (Combo RegC) = [6]
  toWords (None l) = [l]

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
  write :: Word3 -> m ()
