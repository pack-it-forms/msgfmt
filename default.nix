{ args ? {} }:

let
  _pkgs = import <nixpkgs> {};

  defaultArgs = self: {
    args = args;
    nixpkgs = _pkgs;
    hsp = self.nixpkgs.haskellPackages;
    release = false;
    releaseGHDevLocal =
       { owner, repo, rev, sha256, local
       , ghPath ? ./., fetch ? self.nixpkgs.fetchFromGitHub }:
         if self.release
          then import (fetch { inherit owner repo rev sha256; } + ghPath)
          else import local;
  };

  projectDeps = self: super:
      (import ./project-deps.nix self super);

  withDefaults = _pkgs.lib.fix (self:
    _pkgs.lib.extends projectDeps defaultArgs self
    // args);

  hsDeps = with withDefaults; nixpkgs.lib.filterAttrs
    (n: v: n != "nixpkgs")
    (builtins.intersectAttrs (projectDeps withDefaults withDefaults)
                             withDefaults);

  isPath = x: ! (builtins.isFunction x || builtins.isAttrs x) &&
              withDefaults.nixpkgs.lib.types.path.check x;

  importedHSDeps = with withDefaults; nixpkgs.lib.mapAttrs
    (n: v: let x = if isPath v then import v else v;
           in if builtins.isFunction x then x { args = args; } else x)
    hsDeps;
in with withDefaults;

nixpkgs.lib.overrideDerivation
  (hsp.callCabal2nix (builtins.baseNameOf ./.) ./. importedHSDeps)
  (attrs: { src = nixpkgs.lib.cleanSource ./.; })
