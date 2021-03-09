-- CurryCheck test for the Data2Xml tool
--

import Control.Monad               ( unless )
import Curry.Compiler.Distribution ( installDir )
import Test.Prop

import System.Directory ( getHomeDirectory, setCurrentDirectory )
import System.FilePath  ( (</>) )
import System.Process   ( system )

import PackageConfig    ( packageExecutable )

-- Should we report output from running the tests?
quiet :: Bool
quiet = False

xmldataTool :: String
xmldataTool = packageExecutable

showExecCmd :: String -> IO Int
showExecCmd cmd = do
  unless quiet $ putStrLn $ "EXECUTING: " ++ cmd
  system cmd

testGenerateXMLConversions :: PropIO
testGenerateXMLConversions = execgencmds `returns` 0
 where
  execgencmds = do
    ec1 <- showExecCmd $ xmldataTool ++ " Prelude"
    ec2 <- showExecCmd $ xmldataTool ++ " FlatCurry.Types"
    return (ec1 + ec2)

testXMLDataConversion :: PropIO
testXMLDataConversion = showExecCmd convertCmd `returns` 0
 where
  quietargs = if quiet then ":set -time :set v0 :set parser -Wnone " else ""
  convertCmd = installDir ++ "/bin/curry " ++ quietargs ++
               ":load testData2XmlProg :eval main :quit"

-- Clean:
testCleanup :: PropIO
testCleanup = clean `returns` 0
 where
  clean = do
    cleanCurry "PreludeDataToXml"
    cleanCurry "FlatCurry_TypesDataToXml"
    cleanCurry "testData2XmlProg"
    system "/bin/rm -f PreludeDataToXml.curry FlatCurry_TypesDataToXml.curry"

  cleanCurry m = system $ installDir ++ "/bin/cleancurry " ++ m
