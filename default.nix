{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "sceneries";
  src = ./.;

  buildInputs = [
    pkgs.purescript
    pkgs.nodePackages.pulp
    pkgs.nodePackages.bower
    pkgs.git
    pkgs.nodejs
  ];

    buildCommand = ''

      export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
      export HOME=$(mktemp -d);

      BUILD_DIR=$(mktemp -d);
      mkdir -p $out;

      cd $src

      cp -r bower.json package.json src -t $BUILD_DIR
      cp -r public/* -t $out

      cd $BUILD_DIR

      npm install

      bower install

      pulp browserify > $out/main.js
    '';
}
