module Main (main) where

import Service

main :: IO ()
main = service (Cfg "temp salt" 8080 Info)
