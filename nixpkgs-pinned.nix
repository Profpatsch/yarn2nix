let
  # nixos unstable 2021-05-19
  rev = "667950d4e8b2e7ae6e9fd67ee7c9de6a3271044f";
  sha256 = "00g52s3c1fqxpcs7grca7zdw8yb4780ns2jxf73hkiri2ndmd197";
in

import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  inherit sha256;
})
