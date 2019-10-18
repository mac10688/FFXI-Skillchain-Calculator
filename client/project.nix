
with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "elm-environment";

  buildInputs = [
    elmPackages.elm
    pkgs.nginx
    sass
    nodejs-11_x
  ];
}
