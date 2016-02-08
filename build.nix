let

uwb = import <urweb-build>;

in with uwb;

rec {

  lib = mkLib {
    name = "Prelude";

    statements = [
      (sys "list")
      (src1 ./src/Prelude.ur)
    ];
  };

  tests = [(
    mkExe {
      name = "PreludeTest";
      dbms = "sqlite";
      statements = [
        (lib-local lib)
        (sys "option")
        (src1 ./test/PreludeTest.ur)
      ];
     }
   )];

}


