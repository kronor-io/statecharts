{
  description = "statecharts";
  nixConfig = {
    extra-substituters = "https://pranaysashank.cachix.org";
    extra-trusted-public-keys = "pranaysashank.cachix.org-1:VeqW46y6BVO74w4ViwzeWqSpDqxuWxtC2DO2zoe9rzc=";
  };
  inputs = {
    git-hooks.url = "github:cachix/git-hooks.nix";
    haskell-nix.url = "github:kronor-io/haskell.nix/wip-nixpkgs-ghc";
    haskell-nix.inputs.hackage.follows = "hackage-nix";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    hackage-nix.url = "github:kronor-io/hackage.nix";
    hackage-nix.flake = false;
    rust-overlay.url = "github:oxalica/rust-overlay"; # Add this
  };
  outputs =
    { self
    , git-hooks
    , haskell-nix
    , hackage-nix
    , nixpkgs
    , rust-overlay  # Add this
    }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      mergeAttrs = x: y: x // y;

      foldr = op: nul: list:
        let
          len = builtins.length list;
          fold' = n:
            if n == len
            then nul
            else op (builtins.elemAt list n) (fold' (n + 1));
        in
        fold' 0;

      foldAttrs =
        op:
        nul:
        list_of_attrs:
        foldr
          (n: a:
          foldr
            (name: o:
            o // { ${name} = op n.${name} (a.${name} or nul); }
            )
            a
            (builtins.attrNames n)
          )
          { }
          list_of_attrs;
      eachSystem = f: foldAttrs mergeAttrs { }
        (map (s: builtins.mapAttrs (_: v: { ${s} = v; }) (f s)) supportedSystems);
    in
    eachSystem (system:
    let
      haskellNix = import haskell-nix {
        inherit system;
        sourcesOverride = {
          hackage = hackage-nix;
        };
      };

      overlays = [
        haskellNix.overlay
        rust-overlay.overlays.default # Add rust overlay
      ];

      pkgs = import nixpkgs ({
        inherit system;
        overlays = overlays;
      });

      pristinePkgs = import nixpkgs ({
        inherit system;
      });

      compiler-nix-name = "ghc9122";

      # Define Rust version here
      rustVersion = pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default);
      # OR use a stable version:
      # rustVersion = pkgs.rust-bin.stable."1.85.0".default;
      # OR latest stable:
      # rustVersion = pkgs.rust-bin.stable.latest.default;

      # PostgreSQL dependencies for pgrx
      postgresqlPkgs = with pkgs; [
        # PostgreSQL development headers - REQUIRED for pgrx
        postgresql_16
        postgresql.lib # libpq development library

        # Build tools for pgrx
        clang # Required by pgrx for compilation
        llvmPackages.libclang # More clang libraries
        pkg-config # For finding PostgreSQL libraries

        # General build essentials
        gcc
        gnumake
        binutils

        # OpenSSL development packages
        openssl # The OpenSSL library
        openssl.dev # Development headers and .pc files

        # Readline for PostgreSQL compilation
        readline
        readline.dev

        # ICU library for Unicode suppor
        icu
        icu.dev

        # Other PostgreSQL build dependencies
        bison # Parser generator
        flex # Lexical analyzer
        zlib # Compression library
        zlib.dev

        # Optional but useful for development
        lld # Faster linker (optional)

        # Rust toolchain - use the version we defined above
        rustVersion
      ];

      statechartProject = pkgs.haskell-nix.project {
        src = ./sdk;
        modules = (if system == "x86_64-darwin" || system == "aarch64-darwin" then [ ] else [{
          dontPatchELF = false;
          dontStrip = false;
        }]) ++ [{ doHaddock = false; }];

        inherit compiler-nix-name;
        cabalProjectFreeze = builtins.readFile ./sdk/cabal.project.freeze;
        supportHpack = false;
      };

      shell = statechartProject.shellFor {
        withHoogle = false;

        nativeBuildInputs = postgresqlPkgs;

        shellHook = ''
          ${self.checks.${system}.pre-commit-check.shellHook}

          # Set PostgreSQL paths for pgrx
          export PG_CONFIG="${pkgs.postgresql_16}/bin/pg_config"
          export LIBCLANG_PATH="${pkgs.llvmPackages.libclang.lib}/lib"
          
          # Verify Rust version
          echo "Rust version: $(rustc --version)"
          echo "Cargo version: $(cargo --version)"
        '';
      };

    in
    {
      checks = {
        pre-commit-check = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt.enable = true;
            fourmolu.enable = true;
          };
        };
      };
      packages = {
        default = statechartProject.statechart-sdk.components.exes.generate-chart;
        freezeFile = statechartProject.plan-nix.freeze;
      };
      devShells.default = shell;
    }
    );
}
