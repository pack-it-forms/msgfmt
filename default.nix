{ args ? {} }:

let
  _pkgs = import <nixpkgs> {};

  defaultArgs = self: {
    args = args;
    nixpkgs = _pkgs;
    hsp = self.nixpkgs.haskellPackages;
    hsExtraDeps = {};
    # Convention: devInputs are set in project-deps.nix, extraDevInputs are
    # reserved for easy command-line use.
    devInputs = [];
    extraDevInputs = [];
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
    let x = _pkgs.lib.extends projectDeps defaultArgs self;
    in x // args
         // { hsExtraDeps = x.hsExtraDeps or {} // args.hsExtraDeps or {}; });
in with withDefaults; with nixpkgs.lib; with builtins; let

  isPath = x: ! (isFunction x || isAttrs x) &&
              types.path.check x;

  # There may be more extra deps specified (via args) than the derivation
  # produced from the cabal file actually takes.  This filters them out.  Note
  # that this means that only dependencies "declared" in
  # project-dependencies.nix can be overridden; unfortunately, there is no easy
  # way to get the list of attributes accepted by the cabal2nix-generated
  # derivation, since it is wrapped in several layers of functions after being
  # imported.
  thisHSDeps =
    intersectAttrs (projectDeps withDefaults withDefaults).hsExtraDeps or {}
                   hsExtraDeps;

  importedHSDeps = mapAttrs
    (n: v: let x = if isPath v then import v else v;
           in if isFunction x then x { args = args; } else x)
    thisHSDeps;

  rawDrv = hsp.callCabal2nix (baseNameOf ./.) ./. importedHSDeps;

  cabalDistFilter = name: type:
    let baseName = baseNameOf (toString name); in ! (
      (type == "directory" && (baseName == "dist" || baseName == "dist-newstyle"))
    ) && cleanSourceFilter name type;
  cleanSource = filterSource cabalDistFilter;

  allDevInputs = [ nixpkgs.cabal-install ] ++ devInputs ++ extraDevInputs;
in
rawDrv.overrideAttrs (attrs: {
  src = cleanSource ./.;
  passthru = attrs.passthru // {
    env = attrs.passthru.env.overrideAttrs (attrs: {
      nativeBuildInputs = attrs.nativeBuildInputs ++ allDevInputs;
    });
  };
})
