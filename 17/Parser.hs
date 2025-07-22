module Parser (machineConfiguration) where

import Data.Finite (finite)
import Data.Finite.Integral (packFinite)
import Machine (Combo (..), Instruction (..), MachineConfig (..), Operand (..), SomeInstruction (..), Word3)
import Parsing (eol, fromMaybe, number)
import Text.ParserCombinators.ReadP (ReadP, char, eof, pfail, sepBy, string)

machineConfiguration :: ReadP MachineConfig
machineConfiguration = do
  regA' <- readRegister "A"
  regB' <- readRegister "B"
  regC' <- readRegister "C"
  instructions' <- readProgram <* eof
  return $ MachineConfig regA' regB' regC' instructions'
  where
    readRegister reg = valueByKey ("Register " ++ reg) number

    readProgram = eol *> valueByKey "Program" (sepBy instruction (char ','))

    valueByKey key parser = string (key ++ ": ") *> parser <* eol

    instruction :: ReadP SomeInstruction
    instruction = do
      opcode <- readFinite
      _ <- char ','
      operand <- readFinite
      case opcode of
        0 -> SomeInstruction . Adv <$> combo operand
        1 -> pure . SomeInstruction . Bxl . Literal $ operand
        2 -> SomeInstruction . Bst <$> combo operand
        3 -> pure . SomeInstruction . Jnz . Literal $ operand
        4 -> pure . SomeInstruction . Bxc . None $ operand  -- Save for reconstruction of words
        5 -> SomeInstruction . Out <$> combo operand
        6 -> SomeInstruction . Bdv <$> combo operand
        7 -> SomeInstruction . Cdv <$> combo operand
        _ -> error "Unreachable" -- Compiler cannot infer that Finite 8 only allows values 0-7
      where
        combo :: Word3 -> ReadP (Operand Integer)
        combo operand =
          if operand <= 3
            then (pure . Combo . Lit . finite . toInteger) operand
            else case operand of
              4 -> pure $ Combo RegA
              5 -> pure $ Combo RegB
              6 -> pure $ Combo RegC
              7 -> pfail -- Reserved, invalid combo operand
              _ -> error "Unreachable" -- Compiler cannot infer that Finite 8 only allows values 0-7
        
        readFinite :: ReadP Word3
        readFinite = number >>= fromMaybe . packFinite
