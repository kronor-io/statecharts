{
  description = "statecharts";
  nixConfig = {
    extra-substituters = "https://kronor-open.cachix.org";
    extra-trusted-public-keys = "kronor-open.cachix.org-1:D1shHZh5BRkmM8RB9BaEqBURIgD/n5+u8KFXD1+DbF8=";
  };
  inputs = {
    git-hooks.url = "github:cachix/git-hooks.nix";
    haskell-nix.url = "github:kronor-io/haskell.nix/wip-nixpkgs-ghc";
    haskell-nix.inputs.hackage.follows = "hackage-nix";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    hackage-nix.url = "github:kronor-io/hackage.nix";
    hackage-nix.flake = false;
    nix-github-actions.url = "github:nix-community/nix-github-actions";
    nix-github-actions.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    { self
    , git-hooks
    , haskell-nix
    , hackage-nix
    , nixpkgs
    , nix-github-actions
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
    eachSystem
      (system:
      let
        haskellNix = import haskell-nix {
          inherit system;
          sourcesOverride = {
            hackage = hackage-nix;
          };
        };

        pkgs = import nixpkgs (
          {
            inherit system;
            overlays = [ haskellNix.overlay ];
          }
        );
        pristinePkgs = import nixpkgs (
          {
            inherit system;
          }
        );
        compiler-nix-name = "ghc9122";

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

          shellHook = ''
            ${self.checks.${system}.pre-commit-check.shellHook}
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
      ) // {
      githubActions = nix-github-actions.lib.mkGithubMatrix {
        checks = (nixpkgs.lib.attrsets.recursiveUpdate self.checks self.packages);
        platforms = nix-github-actions.lib.githubPlatforms // {
          "aarch64-darwin" = "macos-15";
        };
      };
    };
}
