{-|
Module : Yarn.Lock
Description : Parser & Types for yarn.lock files
Maintainer : Profpatsch
Stability : experimental

The <https://yarnpkg.com/ Yarn package manager> improves on npm
in that it writes @yarn.lock@ files that contain a complete
version resolution of all dependencies. This way a deterministic
deployment can be guaranteed.
-}
module Yarn.Lock where

-- TODO export general functions from inner modules
