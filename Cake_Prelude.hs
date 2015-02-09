module Cake_Prelude where

import Development.Cake3
import Development.Cake3.Ext.UrWeb
import Cake_Prelude_P

lib = uwlib (file "lib.urp") $ do
  ur (sys "list")
  ur (file "src/Prelude.ur")

test = uwapp "-dbms sqlite" (file "test/PreludeTest.urp") $ do
  let src = file "test/PreludeTest.ur"
  database ("dbname="++((takeBaseName src) .= "db"))
  sql (src .= "sql")
  library lib
  ur (sys "option")
  ur (sys "list")
  ur src
  
main = writeMake (file "Makefile") $ do
  rule $ do
    phony "lib"
    depend lib

  rule $ do
    phony "test"
    depend test

  rule $ do
    phony "all"
    depend lib
    depend test

