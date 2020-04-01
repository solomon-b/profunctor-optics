{ mkDerivation, base, invertible, mtl, profunctors, stdenv
, transformers
}:
mkDerivation {
  pname = "profunctor-optics";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base invertible mtl profunctors transformers
  ];
  homepage = "http://www.github.com/ssbothwell/profunctor-optics";
  description = "A prototype implementation of Profunctor Optics";
  license = stdenv.lib.licenses.bsd3;
}
