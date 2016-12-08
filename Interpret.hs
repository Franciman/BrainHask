module Interpret (execute, execute_) where

import Syntax (Instruction(..), SyntaxTree)
import Data.Char (ord, chr)
import Control.Monad (foldM, foldM_)
import qualified Data.ByteString as BS
import Data.Word (Word8)

import Control.Monad.State

import Memory


execute_ :: SyntaxTree -> IO ()
execute_ instrs = let memory = zipped (repeat 0)
                     in doExecute_ memory instrs

execute :: SyntaxTree -> IO (MemoryZipper Word8)
execute instrs = let memory = zipped (repeat 0)
                     in doExecute memory instrs

doExecute_ :: MemoryZipper Word8 -> SyntaxTree -> IO ()
doExecute_ = foldM_ execute'

doExecute :: MemoryZipper Word8 -> SyntaxTree -> IO (MemoryZipper Word8)
doExecute = foldM execute'
    
readByte :: IO Word8
readByte = BS.head <$> BS.getLine

writeByte :: Word8 -> IO ()
writeByte = BS.putStr . BS.singleton


execute' :: MemoryZipper Word8 -> Instruction -> IO (MemoryZipper Word8)
execute' mem IncrementPointer       = return $ next mem
execute' mem DecrementPointer       = return $ previous mem
execute' mem IncrementByte          = return $ (+1) `apply` mem
execute' mem DecrementByte          = return $ (1 `subtract`) `apply` mem
execute' mem Output                 = writeByte (cell mem) >> return mem
execute' mem Input                  = (flip apply) mem <$> (const <$> readByte)
execute' mem instr@(Squared instrs) = if cell mem == 0
                                         then return mem
                                         else doExecute mem instrs >>= \mem' -> execute' mem' instr
