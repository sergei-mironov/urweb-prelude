{ libraries ? {} } :

let

  uwb = import <urweb-build> {inherit libraries; };

in with uwb;

rec {

  prelude = mkLib {
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

      libraries = {
        inherit prelude;
      };

      statements = [
        (sys "option")
        (src1 ./test/PreludeTest.ur)
      ];
    }
  )];

}


