let
  # nixos unstable 2021-06-23
  rev = "1905f5f2e55e0db0bb6244cfe62cb6c0dbda391d";
  sha256 = "148f79hhya66qj8v5gn7bs6zrfjy1nbvdciyxdm4yd5p8r6ayzv6";
in

import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  inherit sha256;
})
