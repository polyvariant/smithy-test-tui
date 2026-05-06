{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    scala-cli-nix.url = "github:scala-nix/scala-cli-nix/native-support";
    scala-cli-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, scala-cli-nix, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];
    in {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ scala-cli-nix.overlays.default ];
          };
        in {
          default = pkgs.callPackage ./derivation.nix { };
        }
      );

      # Pull `passthru.tests` from every package into checks, so
      # `nix flake check` runs the test scope of each target.
      checks = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ scala-cli-nix.overlays.default ];
          };
        in pkgs.scala-cli-nix.collectChecks self.packages.${system}
      );

      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ scala-cli-nix.overlays.default ];
          };
        in {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.scala-cli
              pkgs.scala-cli-nix-cli
            ];
          };
        }
      );
    };
}
