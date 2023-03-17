{
  inputs = {
    nixpkgs.url       = "github:nixos/nixpkgs/nixos-22.11";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix/main";
  };

  outputs = inputs:
    with inputs; let
      supportedSystems = [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [stacklock2nix.overlay self.overlay];
        });
      unstableNixpkgsFor = forAllSystems (system:
        import unstable {
          inherit system;
          overlays = [];
        });
    in {
      overlay = import nix/overlay.nix;

      packages = forAllSystems (system: {
        baby-black-and-whites = nixpkgsFor.${system}.baby-black-and-whites-app;
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.baby-black-and-whites);

      devShell = forAllSystems (system: nixpkgsFor.${system}.baby-black-and-whites-dev-shell);
    };
}
