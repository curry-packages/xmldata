-- CurryCheck test for Data2Xml tool
--

import Curry.Compiler.Distribution ( installDir )
import Test.Prop

import System.Directory ( getHomeDirectory, setCurrentDirectory )
import System.FilePath  ( (</>) )
import System.Process   ( system )

import PackageConfig (packageExecutable)

xmldataTool :: String
xmldataTool = packageExecutable

testGenerateXMLConversions :: PropIO
testGenerateXMLConversions = init `returns` 0
 where
   init = do system (xmldataTool ++ " Prelude")
             system (xmldataTool ++ " FlatCurry.Types")

testXMLDataConversion :: PropIO
testXMLDataConversion = system convertCmd `returns` 0
 where
  convertCmd = "cypm curry :set -time :set v0 " ++
               ":set parser -Wnone :l testData2XmlProg :eval main :q"

-- Clean:
testCleanup :: PropIO
testCleanup = clean `returns` 0
 where
  clean = do
    system (installDir ++ "/bin/cleancurry PreludeDataToXml")
    system (installDir ++ "/bin/cleancurry FlatCurry_TypesDataToXml")
    system (installDir ++ "/bin/cleancurry testData2XmlProg")
    system "/bin/rm -f PreludeDataToXml.curry FlatCurry_TypesDataToXml.curry"
