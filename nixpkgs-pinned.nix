let
  # nixos unstable 2021-06-23
  rev = "1943fe6bb24631dc884dbd47d33ea5c7019d9857";
  sha256 = "02ibwp0jdbpsv3px4l0nbzzj1hv98gy7zj7cwnkmdsdm00f4y45y";
in

import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  inherit sha256;
})
