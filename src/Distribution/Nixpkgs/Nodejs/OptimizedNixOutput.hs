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
-}
module Distribution.Nixpkgs.Nodejs.OptimizedNixOutput
( convertLockfile
-- * File Structure
-- $fileStructure
, mkPackageSet
-- * NOTE: fix
-- $noteFix
) where

import Protolude
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Fix (Fix(Fix))
import qualified Data.MultiKeyedMap as MKM
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE

import Nix.Expr (NExpr, ($=), (==>), (@@))
import Nix.Expr.Additions (($$=), (!!.), inheritStatic)
import qualified Nix.Expr as N
import qualified Nix.Expr.Additions as NA

import qualified Yarn.Lock.Types as YLT

import qualified Distribution.Nixpkgs.Nodejs.ResolveLockfile as Res
import Distribution.Nixpkgs.Nodejs.Utils (packageKeyToSymbol)

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

-- | Build a nix string from multiple @AStrVal@s.
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
  , registryBuilder :: NVar -> NVar -> [AStrVal]
    -- ^ constructs a nix function that in turn constructs a repository string;
    -- the function takes a package name and package version
  }

data Git = Git
  { gitUrl :: Text
  , gitRev :: Text }

-- | Final package reference used in the generated package list.
data PkgRef
  = PkgRef Text
    -- ^ reference to another package definition (e.g. @^1.2@ points to @1.2@)
  | PkgDefFile (PkgData (Either Text Registry))
    -- ^ actual definition of a file package
  | PkgDefFileLocal (PkgData Text)
    -- ^ actual definition of a local package (tar.gz file relative to nix expression)
  | PkgDefGit  (PkgData Git)
    -- ^ actual definition of a git package

-- | Package definition needed for calling the build function.
data PkgData a = PkgData
  { pkgDataName :: YLT.PackageKeyName -- ^ package name
  , pkgDataVersion :: Text            -- ^ package version
  , pkgDataUpstream :: a              -- ^ points to upstream
  , pkgDataHashSum :: Text            -- ^ the hash sum of the package
  , pkgDataDependencies :: [Text]     -- ^ list of dependencies (as resolved nix symbols)
  }

-- | Tuples of prefix string to registry
registries :: [(Text, Registry)]
registries =
  [ ( yarnP
    , Registry "yarn"
        $ \n v -> [T yarnP, V n, T "/-/", V n, T "-", V v, T ".tgz"] )
  , ( npmjsP
    , Registry "npm"
        $ \n v -> [T npmjsP, V n, T "/-/", V n, T "-", V v, T ".tgz"] )

  ]
  where
    yarnP = "https://registry.yarnpkg.com/"
    npmjsP = "https://registry.npmjs.org/"

shortcuts :: M.Map [NSym] NSym
shortcuts = M.fromList
  [ (["self"], "s")
  , (["registries", "yarn"], "y")
  , (["registries", "npm"], "n")
  , (["nodeFilePackage"], "f")
  , (["nodeFileLocalPackage"], "l")
  , (["nodeGitPackage"], "g")
  , (["identityRegistry"], "ir")
  , (["scopedName"], "sc")
  ]

-- | Find out which registry the given 'YLT.Remote' shortens to.
recognizeRegistry :: YLT.PackageKeyName -- ^ package name
                  -> Text -- ^ url to file
                  -> Maybe Registry
-- We don’t shorten scoped key names, because
-- they are handled specially by npm registries and
-- the URLs differ from other packages
recognizeRegistry (YLT.ScopedPackageKey{}) _ = Nothing
recognizeRegistry _ fileUrl = snd <$> foundRegistry
  where
    -- | Get registry by the prefix of the registry’s URL.
    foundRegistry = find predicate registries
    predicate :: (Text, Registry) -> Bool
    predicate reg = fst reg `T.isPrefixOf` fileUrl


-- | Convert a 'Res.ResolvedLockfile' to its final, nix-ready form.
convertLockfile :: Res.ResolvedLockfile -> M.Map Text PkgRef
convertLockfile = M.fromList . foldMap convert . MKM.toList
  where
    -- | For the list of package keys we generate a @PkgRef@ each
    -- and then one actual @PkgDef@.
    convert :: (NE.NonEmpty YLT.PackageKey, (Res.Resolved YLT.Package))
            -> [(Text, PkgRef)]
    convert (keys, Res.Resolved{ hashSum, resolved=pkg }) = let
      -- | Since there might be more than one key name, we choose
      -- the one with most entries.
      defName = NE.head $ maximumBy (comparing length) $ NE.group $ NE.sort $ fmap YLT.name keys
      defSym = packageKeyToSymbol $ YLT.PackageKey
        { YLT.name = defName
        , YLT.npmVersionSpec = YLT.version pkg }
      pkgDataGeneric upstream = PkgData
        { pkgDataName = defName
        , pkgDataVersion = YLT.version pkg
        , pkgDataUpstream = upstream
        , pkgDataHashSum = hashSum
        , pkgDataDependencies = map packageKeyToSymbol
            -- TODO: handle optional dependencies better
            $ YLT.dependencies pkg <> YLT.optionalDependencies pkg
        }
      def = case YLT.remote pkg of
        YLT.FileRemote{fileUrl} ->
          PkgDefFile $ pkgDataGeneric $ note fileUrl
            $ recognizeRegistry defName fileUrl
        YLT.FileLocal{fileLocalPath} ->
          PkgDefFileLocal $ pkgDataGeneric $ fileLocalPath
        YLT.GitRemote{gitRepoUrl, gitRev} ->
          PkgDefGit $ pkgDataGeneric $ Git gitRepoUrl gitRev
                 -- we don’t need another ref indirection
                 -- if that’s already the name of our def
      refNames = List.delete defSym $ toList $ NE.nub
        $ fmap packageKeyToSymbol keys
      in (defSym, def) : map (\rn -> (rn, PkgRef defSym)) refNames


{- $fileStructure

@
{ fetchgit, fetchurl }:
# self & super: see notes on fix
self: super:
let
  # shorten the name of known package registries
  registries = {
    yarn = n: v: "https://registry.yarnpkg.com/${n}/-/${n}-${v}.tgz";
  };

  # We want each package definition to be one line, by putting
  # the boilerplate into these functions for different remotes.
  nodeFilePackage = …
  nodeFileLocalPackage = …
  nodeGitPackage = …

  # an identity function for e.g. git repos or unknown registries
  identityRegistry = url: _: _: url;

  # a way to pass through scoped package names
  scopedName = scope: name: { inherit scope name; }

  # shortcut section
  s = self;
  ir = identityRegistry;
  f = nodeFilePackage;
  l = nodeFileLocalPackage;
  g = nodeGitPackage;
  y = registries.yarnpkg;
  sc = scopedName;
  …

# the actual package definitions
in {
  "accepts@~1.3.3" = s."accepts@1.3.3";
  "accepts@1.3.3" = f "accepts" "1.3.3" y "sha" [];
  "@types/accepts@1.3.3" = f (sc "types" "accepts") "1.3.3" y "sha" [];
  "babel-core@^6.14.0" = s."babel-core@6.24.1";
  "babel-core@6.24.1" = f "babel-core" "6.24.1" y "a0e457c58ebdbae575c9f8cd75127e93756435d8" [
    s."accepts@~1.3.3"
  ];
  "local-package@file:../foo.tgz" = l "local-package" "file:../foo.tgz" ../foo.tgz "thehash" []
}
@
-}

-- | Convert a list of packages prepared with 'convertLockfile'
-- to a nix expression.
mkPackageSet :: M.Map Text PkgRef -> NExpr
mkPackageSet packages =
  NA.simpleParamSet ["fetchurl", "fetchgit"]
    -- enable self-referencing of packages
    -- with string names with a self/super fix
    -- see note FIX
    ==> N.Param "self" ==> N.Param "super"
    ==> N.mkLets
        (  [ "registries" $= N.mkNonRecSet (fmap (mkRegistry . snd) registries)
           , "nodeFilePackage" $= buildPkgFn
           , "nodeFileLocalPackage" $= buildPkgLocalFn
           , "nodeGitPackage" $= buildPkgGitFn
           , "identityRegistry" $= NA.multiParam ["url", "_", "_"] "url"
           , "scopedName" $=
               (NA.multiParam ["scope", "name"]
                 $ N.mkNonRecSet [ inheritStatic ["scope", "name"] ])
           ]
        <> fmap mkShortcut (M.toList shortcuts) )
        (N.mkNonRecSet (map mkPkg $ M.toAscList packages))
  where
    mkRegistry (Registry{..}) = unNSym registrySym $=
      (N.Param "n" ==> N.Param "v" ==> antiquote (registryBuilder "n" "v"))

    concatNSyms :: [NSym] -> NExpr
    concatNSyms [] = panic "non-empty shortcut syms!"
    concatNSyms (l:ls) = foldl (!!.) (N.mkSym $ unNSym l) (fmap unNSym ls)
    mkShortcut :: ([NSym], NSym) -> N.Binding NExpr
    mkShortcut (nSyms, short) = unNSym short $= concatNSyms nSyms
    -- | Try to shorten sym, otherwise use input.
    shorten :: [NSym] -> NExpr
    shorten s = maybe (concatNSyms s) (N.mkSym . unNSym) $ M.lookup s shortcuts
    -- | Build function boilerplate the build functions share in common.
    buildPkgFnGeneric :: [Text] -> NExpr -> NExpr
    buildPkgFnGeneric additionalArguments srcNExpr =
      NA.multiParam (["key", "version"] <> additionalArguments <> ["deps"])
        $ ("super" !!. "_buildNodePackage") @@ N.mkNonRecSet
          [ inheritStatic ["key", "version"]
          , "src" $= srcNExpr
          , "nodeBuildInputs" $= "deps" ]
    -- | Building a 'YLT.FileRemote' package.
    buildPkgFn :: NExpr
    buildPkgFn =
      buildPkgFnGeneric ["registry", "sha1"]
        ("fetchurl" @@ N.mkNonRecSet
          [ "url" $= ("registry" @@ "key" @@ "version")
          , inheritStatic ["sha1"] ])
    -- | Building a 'YLT.FileLocal' package.
    buildPkgLocalFn :: NExpr
    buildPkgLocalFn =
      buildPkgFnGeneric ["path", "sha1"]
        ("builtins.path" @@ N.mkNonRecSet
          [ inheritStatic ["path"]
          -- TODO: use the sha1 here! (does builtins.path only take sha256?)
          -- , "sha256" $= "sha1"
          ])
    -- | Building a 'YLT.GitRemote' package.
    buildPkgGitFn :: NExpr
    buildPkgGitFn =
      buildPkgFnGeneric ["url", "rev", "sha256"]
        ("fetchgit" @@ N.mkNonRecSet
          [ inheritStatic ["url", "rev", "sha256"] ])

    -- | Create a package definition.
    mkPkg :: (Text, PkgRef) -> N.Binding NExpr
    mkPkg (key, pkgRef) = key $$= case pkgRef of
      PkgRef t -> N.mkSym selfSym !!. t
      PkgDefFile pd@PkgData{pkgDataUpstream, pkgDataHashSum} ->
        mkDefGeneric pd "nodeFilePackage"
          [ either (\url -> shorten ["identityRegistry"] @@ N.mkStr url )
                   (\reg -> shorten ["registries", registrySym reg])
                   pkgDataUpstream
          , N.mkStr pkgDataHashSum ]
      PkgDefFileLocal pd@PkgData{pkgDataUpstream = path, pkgDataHashSum} ->
        mkDefGeneric pd "nodeFileLocalPackage" [ N.mkPath False (toS path), N.mkStr pkgDataHashSum ]
      PkgDefGit pd@PkgData{pkgDataUpstream = Git{..}, pkgDataHashSum} ->
        mkDefGeneric pd "nodeGitPackage"
          [ N.mkStr gitUrl, N.mkStr gitRev, N.mkStr pkgDataHashSum ]

    -- | The common parts of creating a package definition.
    mkDefGeneric :: PkgData a -> NSym -> [NExpr] -> NExpr
    mkDefGeneric PkgData{..} buildFnSym additionalArguments =
      foldl' (@@) (shorten [buildFnSym])
        $ [ case pkgDataName of
              YLT.SimplePackageKey n -> N.mkStr n
              YLT.ScopedPackageKey s n -> "sc" @@ N.mkStr s @@ N.mkStr n
          , N.mkStr pkgDataVersion ]
          <> additionalArguments <>
          [ N.mkList $ map (N.mkSym selfSym !!.) pkgDataDependencies ]

    selfSym :: Text
    selfSym = "s"

{- $noteFix

@
self: super:
@

follows the fixpoint scheme first introduced
by the @haskellPackage@ set in @nixpkgs@.
See the @Overlays@ documentation in the @nixpkgs@
manual for explanations of how this works.

Note: originally, this was a shallow fix like

@
let attrs = self: {
    "foo bar" = 1;
    bar = self."foo bar" + 2;
  };
in fix attrs
@

which was just in place to work around referencing
attrset attributes through string names.
The new method is a lot more general and allows deep
overrides of arbitrary packages in the dependency set.
-}
