{ mkDerivation, aeson, base, bytestring, HaskellNet, HaskellNet-SSL
, lens, mime-mail, postgresql-simple, scientific, split, stdenv
, text, time, wreq
}:
mkDerivation {
  pname = "balcony";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring HaskellNet HaskellNet-SSL lens mime-mail
    postgresql-simple scientific split text time wreq
  ];
  executableHaskellDepends = [
    aeson base bytestring HaskellNet HaskellNet-SSL lens mime-mail
    postgresql-simple scientific split text time wreq
  ];
  testHaskellDepends = [
    aeson base bytestring HaskellNet HaskellNet-SSL lens mime-mail
    postgresql-simple scientific split text time wreq
  ];
  homepage = "https://github.com/borkdude/balcony#readme";
  license = stdenv.lib.licenses.bsd3;
}
