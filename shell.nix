{ args ? {} }:
(import ./default.nix { inherit args; }).env
