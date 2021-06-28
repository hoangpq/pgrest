let
  pkgs = import <nixpkgs> { };
  pgrest = pkgs.haskellPackages.callPackage ./default.nix { };
  hsTools = with pkgs.haskellPackages; [
    cabal-install hpack
    hlint
  ];
in
  pkgs.lib.overrideDerivation pgrest.env (old: {
    buildInputs = old.buildInputs ++ [ pkgs.zlib ] ++ hsTools;
  })
