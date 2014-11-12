module Cake_Prelude where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cake_Prelude_P

thelib = uwlib (file "lib.urp") $ do
  ur (sys "list")
  ur (single (file "src/Prelude.ur"))
  
main = writeMake (file "Makefile") $ do

  l <- thelib

  let src = file "test/PreludeTest.ur"
  t <- uwapp "-dbms sqlite" (file "PreludeTest.urp") $ do
    database ("dbname="++((takeBaseName src) .= "db"))
    sql (src .= "sql")
    library l
    ur (sys "option")
    ur (sys "list")
    ur (single src)

  rule $ do
    phony "lib"
    depend l

  rule $ do
    phony "test"
    depend t

  rule $ do
    phony "all"
    depend l
    depend t

