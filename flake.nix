{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        commonInputs = (with pkgs; [ ghc cabal-install ]);
        darwinExtras = pkgs.lib.optionals pkgs.stdenv.isDarwin
          (with pkgs.darwin.apple_sdk.frameworks; [ ]);
        linuxExtras = pkgs.lib.optionals pkgs.stdenv.isLinux (with pkgs; [ ]);
        allInputs = commonInputs ++ darwinExtras ++ linuxExtras;
      in { devShell = pkgs.mkShell { buildInputs = allInputs; }; });
}
