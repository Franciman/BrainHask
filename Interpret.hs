module Interpret(execute) where

import Syntax (Instruction(..), SyntaxTree)
import Data.Char (ord, chr)
import Control.Monad (foldM, foldM_)
import qualified Data.ByteString as BS
import Data.Word (Word8)

import Control.Monad.State

import Memory

type Interpreter = StateT (MemoryZipper Word8) IO

execute :: SyntaxTree -> IO ()
execute syn = evalStateT (interpret syn) memory
    where memory = zipped (repeat 0)


interpret :: SyntaxTree -> Interpreter ()
interpret = mapM_ interpretInstr
   
readByte :: IO Word8
readByte = BS.head <$> BS.getLine

writeByte :: Word8 -> IO ()
writeByte = BS.putStr . BS.singleton

interpretInstr :: Instruction -> Interpreter ()
interpretInstr IncrementPointer       = modify next
interpretInstr DecrementPointer       = modify previous
interpretInstr IncrementByte          = modify $ updateCell (+1)
interpretInstr DecrementByte          = modify $ updateCell (1 `subtract`)
interpretInstr Output                 = gets cell >>= \byte -> lift (writeByte byte)
interpretInstr Input                  = lift readByte >>= \byte -> modify $ updateCellWithValue byte
interpretInstr loop@(Squared instrs) = do
    byteData <- gets cell
    if (byteData == 0)
       then return ()
       else interpret instrs >> interpretInstr loop
