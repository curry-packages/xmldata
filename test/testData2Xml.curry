-- CurryCheck test for the Data2Xml tool
--

import Curry.Compiler.Distribution ( installDir )
import Test.Prop

import System.Directory ( getHomeDirectory, setCurrentDirectory )
import System.FilePath  ( (</>) )
import System.Process   ( system )

import PackageConfig    ( packageExecutable )

xmldataTool :: String
xmldataTool = packageExecutable

testGenerateXMLConversions :: PropIO
testGenerateXMLConversions = init `returns` 0
 where
   init = do system $ xmldataTool ++ " Prelude"
             system $ xmldataTool ++ " FlatCurry.Types"

testXMLDataConversion :: PropIO
testXMLDataConversion = system convertCmd `returns` 0
 where
  convertCmd = installDir ++ "/bin/curry :set -time :set v0 " ++
               ":set parser -Wnone :l testData2XmlProg :eval main :q"

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
