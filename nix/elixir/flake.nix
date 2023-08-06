{

  description = "A very basic flake";
  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/nixos-22.11"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      inherit (pkgs.lib) optional optionals;
      pkgs = import nixpkgs {
        system = "x86_64-linux";   # or something else
        config = { allowUnfree = true; };
      };
      beamPkg = pkgs.beam.packagesWith pkgs.erlangR25;

     elixir = beamPkg.elixir.override {
        version = "1.14.0";
        sha256 = "NJQ2unK7AeLGfaW/hVXm7yroweEfudKVUa216RUmLJs=";
      };
    in
    with pkgs;
    {
      devShell = pkgs.mkShell {
        buildInputs = [
          elixir
          elixir_ls
          glibcLocales
        ] ++ optional stdenv.isLinux inotify-tools
          ++ optional stdenv.isDarwin terminal-notifier
          ++ optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
            CoreFoundation
            CoreServices
          ]);
      };
    });

}
