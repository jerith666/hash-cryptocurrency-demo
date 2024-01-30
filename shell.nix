{ sources ? import nix/sources.nix }:

let
  nixpkgs = import sources.nixpkgs { };
in

let
  inherit (nixpkgs) pkgs;

  lamdera = with nixpkgs.pkgs; import ./nix/lamdera.nix { inherit fetchurl stdenv lib; };

  # pinned to recent (but cached) ancestor of
  # dadc08be jetbrains.idea-{community,ultimate}: 2021.3.2 → 2022.1
  olderIdea = (import sources.olderIdeaNixpkgs { }).jetbrains.idea-community;

  elmPlugin = pkgs.fetchurl {
    url = "https://github.com/utiliteez/intellij-elm/releases/download/v5.0.0-beta21/Elm.IntelliJ-5.0.0-beta21.zip";
    hash = "sha256-JkYNZG/H4BMxJDBxlZ14BB7I92qqDo2zcbd90/kILIg=";
  };

in
pkgs.stdenv.mkDerivation {
  name = "env-0";
  buildInputs = with pkgs;
    [
      # elm
      elmPackages.elm

      olderIdea
      lamdera

      # nix
      niv
      nixpkgs-fmt
      nixd
    ];

  shellHook = "echo elm plugin: ${elmPlugin}";
}
