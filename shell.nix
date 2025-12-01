{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [ pkgs.emacs pkgs.notmuch pkgs.git pkgs.gnugrep pkgs.coreutils pkgs.gnutar pkgs.gzip pkgs.ripgrep ];
}
