{ args ? {} }:

let
  pkg = import ./. { args = { release = true; } // args; };
in builtins.listToAttrs [ { name = pkg.pname; value = pkg; } ]
