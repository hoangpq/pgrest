let
  pkgs = import <nixpkgs> { };
  pgrest = pkgs.haskellPackages.callPackage ./default.nix { };
  hsTools = with pkgs.haskellPackages; [
    cabal-install hpack 
    haskell-dap ghci-dap haskell-debug-adapter phoityne-vscode 
    hlint haskell-language-server
  ];
in
  pkgs.lib.overrideDerivation pgrest.env (old: {
    buildInputs = old.buildInputs ++ [ pkgs.zlib ] ++ hsTools;
  })
