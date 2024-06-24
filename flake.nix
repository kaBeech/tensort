{
  description = "Tensort development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in
  {
    devShells.${system}.default = 
      pkgs.mkShell 
        {
          buildInputs = with pkgs; [
            ghc
            haskellPackages.cabal-install
          ];

          shellHook = ''
            cabal update
          '';
        };
  };
}
