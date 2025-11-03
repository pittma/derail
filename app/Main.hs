module Main (main) where

import Lib

main :: IO ()
main = site (Cfg "temp salt" 8080 Info) (Logger Info)
