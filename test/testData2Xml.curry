-- CurryCheck test for Data2Xml tool
--

import Directory(getHomeDirectory, setCurrentDirectory)
import Distribution(installDir)
import FilePath((</>))
import System(system)
import Test.Prop

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
  convertCmd = installDir++"/bin/curry --noreadline :set -time :set v0 " ++
               ":set parser -Wnone :l testData2XmlProg :eval main :q"

-- Clean:
testCleanup :: PropIO
testCleanup = clean `returns` 0
 where
  clean = do
    system (installDir++"/bin/cleancurry PreludeDataToXml")
    system (installDir++"/bin/cleancurry FlatCurry_TypesDataToXml")
    system (installDir++"/bin/cleancurry testData2XmlProg")
    system "/bin/rm -f PreludeDataToXml.curry FlatCurry_TypesDataToXml.curry"
