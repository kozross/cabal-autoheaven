{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.Autotools
  ( -- * Types
    LibraryName,

    -- * Functions
    buildAutotoolsLibrary,
  )
where

import Control.Exception.Safe (IOException, handleIO)
import Control.Monad.Extra (ifM, unlessM)
import Distribution.Simple (Args)
import Distribution.Simple.Program.Builtin (arProgram, gccProgram)
import Distribution.Simple.Program.Db (needProgram)
import Distribution.Simple.Program.Types
  ( ConfiguredProgram,
    ProgramLocation (FoundOnSystem, UserSpecified),
    programLocation,
  )
import Distribution.Simple.Setup (ConfigFlags)
import Distribution.Types.LocalBuildInfo
  ( LocalBuildInfo,
    buildDir,
    withPrograms,
  )
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Verbosity (normal)
import System.Directory
  ( copyFile,
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    findExecutable,
    findFile,
    makeAbsolute,
    withCurrentDirectory,
  )
import System.Exit (die)
import System.FilePath ((<.>), (</>))
import System.Process (callCommand)

-- | @since 1.0.0
newtype LibraryName = LibraryName FilePath
  deriving
    ( -- | @since 1.0.0
      Eq
    )
    via FilePath
  deriving stock
    ( -- | @since 1.0.0
      Show
    )

-- | @since 1.0.0
buildAutotoolsLibrary ::
  -- | What the library is called.
  LibraryName ->
  -- | Where the source code can be found (relative to project directory).
  FilePath ->
  -- | Required by Cabal.
  Args ->
  -- | Required by Cabal.
  ConfigFlags ->
  -- | Required by Cabal.
  PackageDescription ->
  -- | Required by Cabal.
  LocalBuildInfo ->
  -- | Works entirely via side effects.
  IO ()
buildAutotoolsLibrary name loc _ _ _ buildInfo = do
  destinationPath <- makeAbsolute . buildDir $ buildInfo
  let autotoolsInstallDir = destinationPath </> "autotools-target"
  let resultPath = destinationPath </> toLibName name
  handleIO reportAndVomit $ do
    -- Check if we have an install dir already
    libMaybe <-
      ifM
        (doesDirectoryExist autotoolsInstallDir)
        -- If yes, check if we've already got one
        (tryFindLib autotoolsInstallDir name)
        -- If not, create it
        (createDirectory autotoolsInstallDir >> pure Nothing)
    case libMaybe of
      Just libLoc ->
        unlessM
          (doesFileExist resultPath)
          (copyFile libLoc resultPath)
      Nothing -> withCurrentDirectory loc $ do
        let progDb = withPrograms buildInfo
        ccMaybe <- needProgram normal gccProgram progDb
        case ccMaybe of
          Nothing -> die "Could not find C compiler, giving up."
          Just (cc, progDb') -> do
            arMaybe <- needProgram normal arProgram progDb'
            case arMaybe of
              Nothing -> die "Could not find ar, giving up."
              Just (ar, _) -> do
                shMaybe <- findExecutable "sh"
                case shMaybe of
                  Nothing -> die "Could not find sh, giving up."
                  Just sh -> do
                    makeMaybe <- findExecutable "make"
                    case makeMaybe of
                      Nothing -> die "Could not find make, giving up."
                      Just make -> do
                        configureMakeInstall autotoolsInstallDir cc ar sh make
                        installedLib <- tryFindLib autotoolsInstallDir name
                        case installedLib of
                          Nothing -> die "Build succeeded, but no library found, giving up."
                          Just libLoc -> copyFile libLoc resultPath

-- Helpers

configureMakeInstall ::
  FilePath ->
  ConfiguredProgram ->
  ConfiguredProgram ->
  FilePath ->
  FilePath ->
  IO ()
configureMakeInstall prefix cc ar shPath makePath = do
  let ccPath = go . programLocation $ cc
  let arPath = go . programLocation $ ar
  let configureCommand =
        "CC=" <> ccPath
          <> " AR="
          <> arPath
          <> " "
          <> shPath
          <> "configure --prefix="
          <> prefix
          <> " --disable-shared --enable-static"
  let makeCommand = makePath
  let installCommand = makePath <> " install"
  callCommand configureCommand
  callCommand makeCommand
  callCommand installCommand
  where
    go :: ProgramLocation -> FilePath
    go = \case
      UserSpecified fp -> fp
      FoundOnSystem fp -> fp

reportAndVomit :: IOException -> IO ()
reportAndVomit = die . show

-- This is currently a 'shallow' search.
tryFindLib :: FilePath -> LibraryName -> IO (Maybe FilePath)
tryFindLib loc = findFile [loc] . toLibName

toLibName :: LibraryName -> FilePath
toLibName (LibraryName fp) = "lib" <> fp <.> "a"
