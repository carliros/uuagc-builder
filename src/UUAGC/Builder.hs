module UUAGC.Builder ( defaultUuagcHook) where

import           Control.Exception                     (throwIO)
import           Control.Monad
import           Data.Maybe
import qualified Data.Text                             as T
import           System.Directory
import           System.Exit                           (ExitCode (..))

import           Distribution.PackageDescription
import qualified Distribution.PackageDescription.Parse as PD (readPackageDescription)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import qualified Distribution.Verbosity                as Verbosity

import           UU.UUAGC                              (uuagc)

import           UUAGC.ParserOptions                   (parseFileOptions)

-- | Default parsing of a Cabal file.
readPackageDescription :: FilePath -> IO PackageDescription
readPackageDescription = PD.readPackageDescription Verbosity.silent >=> return . packageDescription

-- | Compile code
build :: PackageDescription -> IO ()
build packageDesc = do
  let fileOptions  = listFieldTuple_ "x-uuagc-file-option"  packageDesc

  forM_ (zip fileOptions [(1::Int)..]) $ \(fileOption, i) -> do
    let file = fst fileOption
        opts = snd fileOption
    exists <- doesFileExist (fst fileOption)
    if exists
      then do
        putStrLn $ "uuagc-builder: " ++ show i ++ " - building " ++ file ++ " " ++ show opts
        runUuagcBuilder fileOption
      else
        error $ "uuagc-builder: Could not find " ++ file

runUuagcBuilder :: (FilePath, [String]) -> IO()
runUuagcBuilder (filePath, opts)
  = do (errorCode, _) <- uuagc opts filePath
       case errorCode of
          ExitSuccess        -> return ()
          ex@(ExitFailure _) -> throwIO ex

-- | Try to read file and options
listFieldTuple_ :: String -> PackageDescription -> [(String, [String])]
listFieldTuple_ fn = fromMaybe [] . listFieldTuple fn

listFieldTuple :: String -> PackageDescription -> Maybe [(String, [String])]
listFieldTuple key = fmap parseFileOptions . field key

-- | Try to read a field's value
field :: String -> PackageDescription -> Maybe String
field key = fmap strip . lookup key . customFieldsPD

-- | Default build hook for your Setup.hs
defaultUuagcHook :: IO ()
defaultUuagcHook
  = defaultMainWithHooks simpleUserHooks { --hookedPreProcessors = ("ag", ag):("lag",ag):knownSuffixHandlers,
                                           buildHook = myBuildHook
                                         }

-- | Default post build hook for your Setup.hs
myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook packageDesc localBuildInfo userHooks buildFlags
  = do --let classesPath = buildDir localBuildInfo </> agClassesFile
       putStrLn "Preprocesing UUAGC files ..."
       build packageDesc
       putStrLn "Finished preprocesing UUAGC files"
       originalBuildHook packageDesc localBuildInfo userHooks buildFlags

originalBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
originalBuildHook = buildHook simpleUserHooks


-- | File used to store de classes defined in the cabal file.
agClassesFile :: String
agClassesFile = "ag_file_options"

-- | Strip leading and trailing whitespace
strip :: String -> String
strip = T.unpack . T.strip . T.pack
