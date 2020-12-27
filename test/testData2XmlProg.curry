-- Test for Data2Xml tool
--
-- Execute:
-- > cypm exec curry-data2xml Prelude
-- > cypm exec curry-data2xml FlatCurry
-- > cypm curry :load testData2XmlProg :eval main :quit

import FlatCurry.Files
import FlatCurry_TypesDataToXml
import XML
import System.Process ( exitWith )

main :: IO ()
main = do
  prog <- readFlatCurry "testData2XmlProg"
  let nprog = xmlToProg (head (parseXmlString (showXmlDoc (progToXml prog))))
  exitWith $ if prog == nprog then 0 else 1
