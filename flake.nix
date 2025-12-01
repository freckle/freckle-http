{
  inputs = {
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    freckle.url = "github:freckle/flakes?dir=main";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        nixpkgs-stable = inputs.nixpkgs-stable.legacyPackages.${system};
        freckle = inputs.freckle.packages.${system};
        freckleLib = inputs.freckle.lib.${system};
      in
      {
        devShells.default = nixpkgs-stable.mkShell {
          buildInputs = [ nixpkgs-stable.zlib ];
          nativeBuildInputs = [
            freckle.fourmolu-0-17-x
            (freckleLib.haskellBundle {
              ghcVersion = "ghc-9-10-3";
              enableHLS = true;
            })
          ];
          shellHook = ''export STACK_YAML=stack.yaml'';
        };

        devShells.nightly = nixpkgs-stable.mkShell {
          buildInputs = [ nixpkgs-stable.zlib ];
          nativeBuildInputs = [
            freckle.fourmolu-0-17-x
            nixpkgs-stable.haskell.compiler.ghc9122
            nixpkgs-stable.stack
          ];
          shellHook = ''export STACK_YAML=stack-nightly.yaml'';
        };
      }
    );
}
