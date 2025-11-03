module Main (main) where

import Service
import Types

main :: IO ()
main = service (Cfg "temp salt" 8080 Info) (Logger Info)
