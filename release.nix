let
  projectPkgs = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = new: old: rec {
        sdtpl = old.callPackage ./sdtpl.nix {};
        pack-it-forms-msgfmt = old.callPackage ./default.nix {};
      };
    };
  };

  pkgs = import <nixpkgs> { overlays = [ projectPkgs ]; };
in
  { pack-it-forms-msgfmt = pkgs.haskellPackages.pack-it-forms-msgfmt; }
