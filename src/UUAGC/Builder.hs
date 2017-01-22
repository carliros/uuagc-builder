module UUAGC.Builder
 ( readPackageDescription
 , build
 , listField
 , listField_
 , field
 , field_
 , readField
 , defaultUuagcHook
 ) where

import Control.Monad
import Data.List.Split
import Data.Maybe
import System.Directory
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import Control.Exception (throwIO)

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Safe
import qualified Distribution.PackageDescription.Parse as PD (readPackageDescription)
import qualified Distribution.Verbosity                as Verbosity

import UU.UUAGC (uuagc)

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
        putStrLn $ show fileOption
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

-- | Try to read a comma separated field
listField :: String -> PackageDescription -> Maybe [String]
listField key = fmap (map strip . splitOn ",") . field key

-- | Read the value of a comma separated field, gives an empty list if the field is not present.
listField_ :: String -> PackageDescription -> [String]
listField_ fn = fromMaybe [] . listField fn

-- | Try to read file and options
listFieldTuple_ :: String -> PackageDescription -> [(String, [String])]
listFieldTuple_ fn = fromMaybe [] . listFieldTuple fn

listFieldTuple :: String -> PackageDescription -> Maybe [(String, [String])]
listFieldTuple key = fmap (map tupleOption . splitOn ",") . field key

tupleOption :: String -> (String, [String])
tupleOption str
  = let resSplit = splitOn ":" str
    in if null resSplit then ("", [])
                        else let file = head resSplit
                                 opts = head $ tail resSplit
                             in (strip file, parseOptions opts)
  where parseOptions = map strip . words


-- | Try to read a field's value
field :: String -> PackageDescription -> Maybe String
field key = fmap strip . lookup key . customFieldsPD

-- | Force reading of a field, fails if it doesn't exist
field_ :: String -> PackageDescription -> String
field_ key = fromMaybe (error $ key ++ "is  missing") . field key

readField :: Read a => String -> a -> PackageDescription -> a
readField key d = fromMaybe d . (readMay <=< field key)

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
