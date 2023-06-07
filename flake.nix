{
  description = "Auth service";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  outputs = {
    nixpkgs,
    flake-utils,
    rust-overlay,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [rust-overlay.overlay];
          config.allowUnfree = true;
        };
        rust =
          (pkgs.rustChannelOf {
            date = "2023-05-09";
            channel = "nightly";
          })
          .default
          .override {
            extensions = ["rust-analyzer" "rust-src" "clippy"];
          };
      in rec {
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            rust
            cargo-edit
            cargo-expand
            cargo-watch

            nodePackages_latest.yarn
            nodejs_latest
            tree-sitter
          ];
        };
      }
    );
}
