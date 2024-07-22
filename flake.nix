{
  description = "Immigrant: Database schema description language";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    shelly = {
      url = "github:CertainLach/shelly";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };
  outputs = inputs @ {
    nixpkgs,
    flake-parts,
    shelly,
    rust-overlay,
    crane,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [shelly.flakeModule];
      perSystem = {
        config,
        system,
        pkgs,
        ...
      }: let
        rust =
          pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        craneLib = (crane.mkLib pkgs).overrideToolchain rust;
        sharedDeps = with pkgs; [
          # PG parser
          rustPlatform.bindgenHook
          cmake
        ];
      in rec {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [rust-overlay.overlays.default];
          config.allowUnfree = true;
        };
        packages = {
          default = packages.immigrant;
          immigrant = craneLib.buildPackage {
            pname = "immigrant";
            src = craneLib.cleanCargoSource (craneLib.path ./.);
            strictDeps = true;
            nativeBuildInputs = sharedDeps;
          };
        };
        shelly.shells.default = {
          factory = craneLib.devShell;
          packages = with pkgs;
            [
              cargo-edit
              just

              tree-sitter
              (yarn-berry.override {nodejs = nodejs_latest;})
              nodejs_latest
              # Wanted by node-nan, which is used by tree-sitter, even if js binding is not actually used.
              python3
            ]
            ++ sharedDeps;
        };
        formatter = pkgs.alejandra;
      };
    };
}
