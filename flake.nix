{
  description = "Immigrant: Database schema description language";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    nixpkgs,
    flake-utils,
    rust-overlay,
    crane,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [rust-overlay.overlays.default];
          config.allowUnfree = true;
        };
        rust =
          pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        craneLib = (crane.mkLib pkgs).overrideToolchain rust;
        sharedDeps = with pkgs; [
          # PG parser
          rustPlatform.bindgenHook
          cmake
        ];
      in rec {
        packages = {
          default = packages.immigrant;
          immigrant = craneLib.buildPackage {
            pname = "immigrant";
            src = craneLib.cleanCargoSource (craneLib.path ./.);
            strictDeps = true;
            nativeBuildInputs = sharedDeps;
          };
        };
        devShell = craneLib.devShell {
          packages = with pkgs; [
            alejandra
            cargo-edit

            just

            nodePackages_latest.yarn
            nodejs_latest
            tree-sitter
          ] ++ sharedDeps;
        };
      }
    );
}
