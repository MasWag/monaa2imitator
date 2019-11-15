module Main where

import Data.Text.Lazy

import Data.TimedAutomata.Parser
import Data.TimedAutomata.Imitator

main :: IO ()
main = do
  inputTA <- parse.pack <$> getContents
  case fromMONAA inputTA of 
    Just imitatorModel -> print imitatorModel
    Nothing -> putStrLn "The given TA is invalid."
