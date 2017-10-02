{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, GeneralizedNewtypeDeriving, ViewPatterns, RecordWildCards, LambdaCase, NamedFieldPuns #-}
{-|
Description: Generate an optimized nix file from a resolved @YLT.Lockfile@

We want to generate a nix file with the following attributes:

1. easy to parse by humans
2. as short as possible
3. updating the yarn.lock generates diffs that are as short as possible

Readability means a clear structure, with definitions at the top.

Reducing the filesize means we can’t duplicate any information and keep identifiers very short. This interferes with readability, but can be amended by giving the full names in the static section and then giving them short identifiers in a second section.

Nice diffing includes having line-based output (if possible one line per package/dependency), as well as keeping the order of items stable (alphabetically sorting package names and dependencies).

* File Structure
$file-structure

* NOTE: fix
$note-fix
-}
module Distribution.Nixpkgs.Nodejs.OptimizedNixOutput
( convertLockfile, mkPackageSet
) where

import Protolude
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Fix (Fix(Fix))
import qualified Data.MultiKeyedMap as MKM
import qualified Data.List as List

import Nix.Expr (NExpr, ($=), (==>), (!.), (@@))
import Nix.Expr.Additions (($$=), (!!.))
import qualified Nix.Expr as N
import qualified Nix.Expr.Additions as NA

import qualified Yarn.Lock.Types as YLT

import qualified Distribution.Nixpkgs.Nodejs.ResolveLockfile as Res

-- | Nix symbol.
newtype NSym = NSym { unNSym :: Text }
  deriving (IsString, Ord, Eq)

-- | Nix input variable.
newtype NVar = NVar NSym
  deriving (IsString)

-- | Builder type for simple antiquoted nix strings.
data AStrVal = V NVar
             -- ^ nix antiquoted variable
             | T Text
             -- ^ normal nix string

-- | Build a nix string from multiple 'AStrVal's.
antiquote :: [AStrVal] -> NExpr
antiquote vals = Fix . N.NStr . N.DoubleQuoted
  $ flip map vals $ \case
      T t -> N.Plain t
      V (NVar (NSym t)) -> N.Antiquoted $ N.mkSym t

-- | A registry that we know of and can therefore shorten
-- into a nix function call.
data Registry = Registry
  { registrySym :: NSym
    -- ^ nix symbol used in the output file
  , registryBuilder :: NVar
                    -- ^ package name
                    -> NVar
                    -- ^ package version
                    -> [AStrVal]
    -- ^ constructs a nix function that in turn constructs a repository string
  }

data Git = Git
  { gitUrl :: Text
  , gitRev :: Text }

-- | Final package reference used in the generated package list.
data PkgRef
  = PkgRef Text
    -- ^ reference to another package definition (e.g. @^1.2@ points to @1.2@)
  | PkgDefFile (PkgData (Either Text Registry))
    -- ^ actual definiton of a file package
  | PkgDefGit  (PkgData Git)
    -- ^ actual definiton of a git package

-- | Package definition needed for calling the build function.
data PkgData a = PkgData
  { pkgDataName :: Text           -- ^ package name
  , pkgDataVersion :: Text        -- ^ package version
  , pkgDataUpstream :: a          -- ^ points to upstream
  , pkgDataSha1sum :: Text        -- ^ the sha1sum of the package
  , pkgDataDependencies :: [Text] -- ^ list of dependencies (as resolved nix symbols)
  }


registries :: [(Text, Registry)]
registries =
  [ ( yarnP
    , Registry "yarn"
        $ \n v -> [T yarnP, V n, T "/-/", V n, T "-", V v, T ".tgz"] )
  ]
  where
    yarnP = "https://registry.yarnpkg.com"

shortcuts :: M.Map [NSym] NSym
shortcuts = M.fromList
  [ (["registries", "yarn"], "y")
  , (["nodeFilePackage"], "f")
  , (["nodeGitPackage"], "g")
  , (["identityRegistry"], "ir")
  ]

-- | Find out which registry the given 'YLT.Remote' shortens to.
recognizeRegistry :: Text -> Maybe Registry
recognizeRegistry fileUrl =
  snd <$> filterRegistry fileUrl
  where
    idRegistry url = Registry "idRegistry" (\_ _ -> [ T url ])
    -- | Get registry by the prefix of the registry’s URL.
    filterRegistry url = find (\reg -> fst reg `T.isPrefixOf` url) registries


-- | Convert a 'Res.ResolvedLockfile' to its final, nix-ready form.
convertLockfile :: Res.ResolvedLockfile -> M.Map Text PkgRef
convertLockfile = M.fromList . foldMap convert . MKM.toList
  where
    packageKeyToSymbol YLT.PackageKey{..} = name <> "@" <> npmVersionSpec
    -- | For the list of package keys we generate a 'PkgRef' each
    -- and then one actual 'PkgDef'.
    convert :: ([YLT.PackageKey], (Res.Resolved YLT.Package)) -> [(Text, PkgRef)]
    convert (keys, Res.Resolved{ sha1sum, resolved=pkg }) = let
      -- | Combine the key names in a hopefully stable way
      -- to get a nice(?) def symbol.
      defName = packageKeyToSymbol $ YLT.PackageKey
        { YLT.name = fold $ List.sort $ List.nub $ map YLT.name keys
        , YLT.npmVersionSpec = YLT.version pkg }
      pkgDataGeneric upstream = PkgData
        { pkgDataName = defName
        , pkgDataVersion = YLT.version pkg
        , pkgDataUpstream = upstream
        , pkgDataSha1sum = sha1sum
        , pkgDataDependencies = map packageKeyToSymbol
            -- TODO: handle optional dependencies better
            $ YLT.dependencies pkg <> YLT.optionalDependencies pkg
        }
      def = case YLT.remote pkg of
        YLT.FileRemote{fileUrl} ->
          PkgDefFile $ pkgDataGeneric $ note fileUrl $ recognizeRegistry fileUrl
        YLT.GitRemote{gitRepoUrl, gitRev} ->
          PkgDefGit $ pkgDataGeneric $ Git gitRepoUrl gitRev
                 -- we don’t need another ref indirection
                 -- if that’s already the name of our def
      refNames = List.delete defName $ List.nub
        $ map packageKeyToSymbol keys
      in (defName, def) : map (\rn -> (rn, PkgRef defName)) refNames


{- $file-structure
@@
{ buildNodePackage, fetchgit, fetchurl, fix }:
let
  # shorten common string prefixes, e.g. of known package repos
  prefixes = {
    yarn = n: v: "https://registry.yarnpkg.com/${n}/-/${n}-${v}.tgz";
  };

  # We want each package definition to be one line, by putting
  # the boilerplate into this function
  buildPkg = …

  # shortcut section
  b = buildPkg;
  y = prefixes.yarnpkg;
  …

  # the actual package definitions; see NOTE fix
  pkgs = s: {
    "accepts@~1.3.3" = s."accepts@1.3.3";
    "accepts@1.3.3" = b "accepts" "1.3.3" y "sha" [];
    "babel-core@^6.14.0" = s."babel-core@6.24.1";
    "babel-core@6.24.1" = b "babel-core" "6.24.1" y "a0e457c58ebdbae575c9f8cd75127e93756435d8" [
      s."accepts@~1.3.3"
    ];
  };

in fix pkgs
@@
-}

-- | Convert a list of packages prepared with 'convertLockfile'
-- to a nix expression.
mkPackageSet :: M.Map Text PkgRef -> NExpr
mkPackageSet packages =
  NA.simpleParamSet ["fix", "fetchurl", "fetchgit", "buildNodePackage"]
    ==> N.mkLets
        (  [ "registries" $= N.mkNonRecSet (fmap (mkRegistry . snd) registries)
           , "nodeFilePackage" $= buildPkgFn
           , "nodeGitPackage" $= buildPkgGitFn
           , "identityRegistry" $= NA.multiParam ["url", "_", "_"] "url" ]
        <> fmap mkShortcut (M.toList shortcuts)
        -- enable self-referencing of packages
        -- with string names with a shallow fix
        -- see note FIX
        <> [ "pkgs" $= (N.Param selfSym ==>
               N.mkNonRecSet (map mkPkg $ M.toAscList packages)) ] )
        ("fix" @@ "pkgs")
  where
    mkRegistry (Registry{..}) = unNSym registrySym $=
      (N.Param "n" ==> N.Param "v" ==> antiquote (registryBuilder "n" "v"))

    concatNSyms :: [NSym] -> NExpr
    concatNSyms [] = panic "non-empty shortcut syms!"
    concatNSyms (l:ls) = foldl (!.) (N.mkSym $ unNSym l) (fmap unNSym ls)
    mkShortcut :: ([NSym], NSym) -> N.Binding NExpr
    mkShortcut (nSyms, short) = unNSym short $= concatNSyms nSyms
    -- | Try to shorten sym, otherwise use input.
    shorten :: [NSym] -> NExpr
    shorten s = maybe (concatNSyms s) (N.mkSym . unNSym) $ M.lookup s shortcuts

    -- | Build function boilerplate the build functions share in common.
    buildPkgFnGeneric :: [Text] -> NExpr -> NExpr
    buildPkgFnGeneric additionalArguments srcNExpr =
      NA.multiParam (["name", "version", "sha1"] <> additionalArguments <> ["deps"])
        $ N.mkSym "buildNodePackage" @@ N.mkNonRecSet
          [ N.inherit $ map N.StaticKey ["name", "version"]
          , "src" $= srcNExpr
          , "nodeBuildInputs" $= "deps" ]
    -- | Building a 'YLT.FileRemote' package.
    buildPkgFn :: NExpr
    buildPkgFn =
      buildPkgFnGeneric ["registry"]
        ("fetchurl" @@ N.mkNonRecSet
          [ "url" $= ("registry" @@ "name" @@ "version")
          , N.inherit $ [N.StaticKey "sha1"] ])
    -- | Building a 'YLT.GitRemote' package.
    buildPkgGitFn :: NExpr
    buildPkgGitFn =
      buildPkgFnGeneric ["url", "rev"]
        ("fetchgit" @@ N.mkNonRecSet
          [ N.inherit $ map N.StaticKey ["url", "rev", "sha1"] ])

    mkDefGeneric :: PkgData a -> NSym -> [NExpr] -> NExpr
    mkDefGeneric PkgData{..} buildFnSym additionalArguments =
      foldl' (@@) (shorten [buildFnSym])
        $ [  N.mkStr pkgDataName
          ,  N.mkStr pkgDataVersion
          ,  N.mkStr pkgDataSha1sum ]
          <> additionalArguments <>
          [ N.mkList $ map (N.mkSym selfSym !!.) pkgDataDependencies ]

    mkPkg :: (Text, PkgRef) -> N.Binding NExpr
    mkPkg (key, pkgRef) = key $$= case pkgRef of
      PkgRef t -> N.mkSym selfSym !!. t
      PkgDefFile pd@PkgData{pkgDataUpstream} ->
        mkDefGeneric pd "nodeFilePackage"
          [ either (\url -> shorten ["identityRegistry"] @@ N.mkStr url )
                   (\reg -> shorten ["registries", registrySym reg])
                   pkgDataUpstream ]
      PkgDefGit pd@PkgData{pkgDataUpstream = Git{..}} ->
        mkDefGeneric pd "nodeGitPackage"
          [ N.mkStr gitUrl, N.mkStr gitRev ]

    selfSym :: Text
    selfSym = "s"

{- $note-fix
If attributes in a rec set have string names
it’s impossible to reference them.

rec {
  "foo bar" = 1;
  bar = "foo bar" + 2; # doesn’t work
}

instead, a small fix can be used:

let attrs = self: {
    "foo bar" = 1;
    bar = self."foo bar" + 2;
  };
in fix attrs
-}
