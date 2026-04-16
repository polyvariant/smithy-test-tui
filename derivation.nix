{ scala-cli-nix }:

scala-cli-nix.buildScalaCliApp {
  pname = "smithy-test-tui";
  version = "0.1.0";
  src = ./.;
  lockFile = ./scala.lock.json;
}
