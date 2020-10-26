module Main where

import Lib
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter imageFilter
