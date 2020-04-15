{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.JSString (JSString)
import Frontend.Servant

main :: IO ()
main = case joinStrings "X" "Y" "Z" of
  "1XYZ" -> putStrLn "OK"
  x -> print x

-- Removing this function hides the bug.
-- Changing this function changes the character of the bug.
renderNodeRef :: (Maybe JSString, JSString, JSString) -> JSString
renderNodeRef (a, b, c) = maybe "a" ("A" <>) a <> b <> c
