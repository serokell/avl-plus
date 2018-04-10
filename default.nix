let
  nixpkgs = import "${overlay}/nixpkgs.nix";
  overlay = builtins.fetchGit {
    url = "ssh://git@github.com:/serokell/serokell-ops.git";
    rev = "431e1c27a82c62cd4a89387d083e8ccf0e159fad";
  };
in

with nixpkgs;

buildStack {
  package = "auth-data-structures";
  src = lib.cleanSource ./.;
}
