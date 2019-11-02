{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;
runCommand "dummy" { buildInputs = [ trash-cli rsync flatbuffers ]; } ""
