let
  pkgs = import <nixpkgs> { };

in
  { balcony = pkgs.haskellPackages.callPackage ./default.nix { };
  }
