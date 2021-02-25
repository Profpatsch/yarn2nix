let
  # nixpkgs master 2021-02-25
  rev = "33fe54081404571adf5688e2c405f5a1f1b22ee5";
  sha256 = "044mdnkj3xqfmw6qg11lp6lw01drras1gfq8vlg8ml685ka7051i";
in

import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  inherit sha256;
})
