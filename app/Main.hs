module Main (main) where

import Web.Scotty

import Lib

main :: IO ()
main = scotty 8080 (site (Logger Info))
