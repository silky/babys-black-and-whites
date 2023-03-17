final: prev: rec {
  baby-black-and-whites-stacklock = final.stacklock2nix {
    stackYaml = ../stack.yaml;
  };

  baby-black-and-whites-pkg-set = final.haskell.packages.ghc925.override (oldAttrs: {
    overrides = final.lib.composeManyExtensions [
      (oldAttrs.overrides or (_: _: {}))

      final.baby-black-and-whites-stacklock.stackYamlResolverOverlay

      final.baby-black-and-whites-stacklock.stackYamlExtraDepsOverlay

      final.baby-black-and-whites-stacklock.stackYamlLocalPkgsOverlay

      final.baby-black-and-whites-stacklock.suggestedOverlay

      (hfinal: hprev: {
        # Some tests don't work
        hpack_0_35_0   = final.haskell.lib.dontCheck hprev.hpack_0_35_0;
        hspec-contrib  = final.haskell.lib.dontCheck hprev.hspec-contrib;
        mfsolve        = final.haskell.lib.dontCheck hprev.mfsolve;
        # diagrams-cairo = final.haskell.lib.dontCheck hprev.diagrams-cairo;
        nothunks = final.haskell.lib.dontCheck hprev.nothunks;
      })
    ];

    all-cabal-hashes = final.fetchurl {
      name = "all-cabal-hashes";
      url = let hash = "58337345887bcb5ade89ea77e0eabe6b274cff28";
             in "https://github.com/commercialhaskell/all-cabal-hashes/archive/${hash}.tar.gz";
      sha256 = "sha256-pxXhmJp/aPF5XmepF25KLlH9M6Eu7ZTXKkAPlIwrqws=";
    };
  });

  baby-black-and-whites-app = final.baby-black-and-whites-pkg-set.baby-black-and-whites;

  baby-black-and-whites-dev-shell = final.baby-black-and-whites-pkg-set.shellFor rec {
    packages = haskPkgs: final.baby-black-and-whites-stacklock.localPkgsSelector haskPkgs;

    # Wrap cabal to always run `hpack` first.
    cabalWrapped = final.writers.writeDashBin "cabal" ''
      ${baby-black-and-whites-pkg-set.hpack_0_35_0}/bin/hpack
      ${final.cabal-install}/bin/cabal "$@"
    '';

    nativeBuildInputs = [
      # Use our overridden hpack so we skip the tests
      baby-black-and-whites-pkg-set.hpack_0_35_0

      final.stack

      cabalWrapped
    ];
  };
}

