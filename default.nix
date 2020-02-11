rec {
  sources = import ./nix/sources.nix;
  haskellNixArgs = import sources."haskell.nix";
  pkgs = import sources.nixpkgs haskellNixArgs;
  haskell-nix = pkgs.haskell-nix;

  hs-pkgs = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit {
      name = "avl-plus";
      src = ./.;
    };
  };

  components = with hs-pkgs.AVL.components; [ library tests.avl-tree-sanity ];
  run-tests = hs-pkgs.AVL.checks.avl-tree-sanity;
}
