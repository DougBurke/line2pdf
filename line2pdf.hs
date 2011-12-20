module Main where
import System.Exit
import System.Environment
import Text.LineToPDF
import qualified Data.ByteString.Lazy.Char8 as L

import Control.Monad (when)

usage :: IO ()
usage = do
  putStrLn "Usage: line2pdf [-big5|-gbk|-shiftjis|-euc-jp|-euc-kr] input.txt > output.pdf"
  putStrLn "  (Form feed (^L) in input denotes a pagebreak.)"
  exitWith ExitSuccess
  
main :: IO ()
main = do
    args <- getArgs
    when (null args) usage

    let (enc, input) = case args of
          ("-big5":i:_)     -> (Big5, i)
          ("-gbk":i:_)      -> (GBK, i)
          ("-euc-jp":i:_)   -> (EUC_JP, i)
          ("-euc-kr":i:_)   -> (EUC_KR, i)
          ("-shiftjis":i:_) -> (ShiftJIS, i)
          (i:_)             -> (Latin, i)
          []                -> error "usage error" -- quieten -Wall

    src <- L.readFile input
    lineToPDF $ defaultConfig enc src
