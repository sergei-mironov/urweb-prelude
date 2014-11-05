module Cake_Prelude where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cake_Prelude_P

thelib = uwlib (file "lib.urp") $ do
  ur (sys "list")
  ur (single (file "src/Prelude.ur"))
  
main = writeMake (file "Makefile") $ do

  l <- thelib

  rule $ do
    phony "lib"
    depend l

  rule $ do
    phony "all"
    depend l
