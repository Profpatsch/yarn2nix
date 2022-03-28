let
  # nixos unstable 2022-03-27
  rev = "1f57d3e7224290eebda23fa1c79718d6b8361574";
  sha256 = "0l81fiqgh6fyz9j3y9fd5v7lqzc40f8j5mj9kac3sprs8dqwravq";
in

import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  inherit sha256;
})
