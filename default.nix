{ mkDerivation, base, pure, pure-css, pure-styles, pure-theme, pure-txt, stdenv
}:
mkDerivation {
  pname = "pure-spinners";
  version = "0.8.0.0";
  src = ./.;
  isLibrary = true;
  libraryHaskellDepends = [
    base pure pure-css pure-styles pure-theme pure-txt
  ];
  license = stdenv.lib.licenses.bsd3;
}
