-- CurryCheck test for Data2Xml tool
--

import Directory(getHomeDirectory, setCurrentDirectory)
import Distribution(installDir)
import FilePath((</>))
import System(system)
import Test.Prop

xmldataTool :: IO String
xmldataTool =
  getHomeDirectory >>= return . (</> (".cpm" </> "bin" </> "curry-data2xml"))

testDir :: String
testDir = "test"

testGenerateXMLConversions :: PropIO
testGenerateXMLConversions = init `returns` 0
 where init = do tool <- xmldataTool
                 system (tool ++ " -d " ++ testDir ++ " Prelude")
                 system (tool ++ " -d " ++ testDir ++ " FlatCurry.Types")

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
    setCurrentDirectory testDir
    system (installDir++"/bin/cleancurry PreludeDataToXml")
    system (installDir++"/bin/cleancurry FlatCurry_TypesDataToXml")
    system (installDir++"/bin/cleancurry testData2XmlProg")
    system "/bin/rm -f PreludeDataToXml.curry FlatCurry_TypesDataToXml.curry"
