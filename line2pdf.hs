{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Exit (exitSuccess)
import Text.LineToPDF
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad (when)
import System.Console.CmdArgs

import Paths_line2pdf (version)
import Data.Version (showVersion)

data Arguments = 
  Arguments 
  {
    encoding :: Encoding
  , file :: FilePath
  , fontsize :: Int
  , fontstyle :: FontStyle
  } deriving (Show, Data, Eq, Typeable)

progName :: String
progName = "line2pdf"

summaryText :: String
summaryText = progName ++ " v" ++ showVersion version ++ ", (C) Audrey Tang"

arguments :: Arguments
arguments = 
  Arguments
  {
    encoding = enum 
               [ Latin &= help "Default encoding"
               , Big5 
               , GBK
               , EUC_JP
               , EUC_KR
               , ShiftJIS
               ]
  , fontsize = 12 &= help "Font size in points (default 12)"
  , fontstyle = Standard &= help "Font style (standard, italic, bold, or bolditalic)"
  , file = def &= args &= typFile
  }
  &= program progName
  &= summary summaryText
  &= help "A simple program to convert lines of text into a PDF file."
  &= details [ "Convert a text file into PDF. The only formatting that is supported"
             , "are new lines and page breaks, indicated by a Form Feed character (^L)."
             , "The PDF is written to stdout."
             , ""
             , "The fontstyle option is only supported for the latin encoding."
             , ""
             , "Examples:"
             , "  " ++ progName ++ " in.txt > out.pdf"
             , "  " ++ progName ++ " --shiftjis in.txt > out.pdf"
             , ""
             ]

main :: IO ()
main = do
  let cmode = cmdArgsMode arguments
  Arguments {..} <- cmdArgsRun cmode
  when (null file) $ putStr (show cmode) >> exitSuccess
    
  src <- L.readFile file
  
  let config = defaultConfig encoding src
  lineToPDF $ config { ptSize = fromIntegral fontsize, fontStyle = fontstyle } 
