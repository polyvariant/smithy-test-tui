{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    scala-cli-nix.url = "github:scala-nix/scala-cli-nix";
    scala-cli-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, scala-cli-nix, ... }:
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
