------------------------------------------------------------------------------
--- A generator for XML data conversion.
---
--- If this program is applied to some Curry module,
--- it generates a new Curry module containing conversion functions
--- from and to an XML representation for all data types declared
--- in this module.
---
--- For instance, if `Nat` is a module containing the declaration
---
---     data Nat = Z | S Nat
---
--- applying this program to `Nat` generates a new module `NatDataToXml`
--- containing the implementation of the following operations:
---
---     natToXml :: Nat -> XmlExp
---     xmlToNat :: XmlExp -> Nat
---
--- Hence, one can store a `Nat` term `num` into the file `Nat.xml` by
---
---     writeXmlFile "Nat.xml" (natToXml num)
---
--- provided that the module `XML` is imported. Similarly, one can read
--- the data from this file by
---
---     readXmlFile "Nat.xml" >>= return . xmlToNat
---
--- @author Bernd Brassel, Michael Hanus
--- @version December 2020
------------------------------------------------------------------------------

module Data2Xml where

import Data.Char
import Data.List
import System.Environment   ( getArgs )

import AbstractCurry.Types
import AbstractCurry.Files
import AbstractCurry.Select
import AbstractCurry.Build
import AbstractCurry.Pretty ( showCProg )
import System.CurryPath     ( runModuleAction )
import System.FilePath      ( (</>) )

data Option = LowCase | FileName String | OutDir String
 deriving Eq

main :: IO ()
main = do
  args <- getArgs
  derive (reverse (argsToOptions args))

printUsage :: IO ()
printUsage = putStrLn $ unlines
  [ "Usage:"
  , ""
  , "     data2xml [-l] [-d <dir>] <filename>"
  , ""
  , "Options:"
  , "-l      : make all tags lowercase"
  , "-d <dir>: write conversion program into directory <dir> (default: .)"
  ]

argsToOptions :: [String] -> [Option]
argsToOptions args = case args of
  ["-h"]          -> []
  ["--help"]      -> []
  ["-?"]          -> []
  "-l" : opts     -> LowCase : argsToOptions opts
  "-d" : f : opts -> OutDir f : argsToOptions opts
  [s]             -> [FileName s]
  _               -> []

outDirOf :: [Option] -> String
outDirOf [] = "."
outDirOf (o : os) = case o of OutDir d -> d
                              _        -> outDirOf os

derive :: [Option] -> IO ()
derive []                   = printUsage
derive (LowCase     : _  )  = printUsage
derive (OutDir _    : _  )  = printUsage
derive (FileName fn : opts) = flip runModuleAction fn $ \mname -> do
  CurryProg modName _ _ _  _ ts _ _ <- readCurry mname
  let (specials,types) = if isPrelude modName
                           then (specialFuncs opts, filterSpecials ts)
                           else ([],ts)
      progName = transModName mname
      impTypes = maybeString $ nub $ filter ((/=modName) .fst)
                             $ concatMap requiredTypesTypeDecl types
  imports <- importTypes modName impTypes
  let outfile = outDirOf opts </> progName ++ ".curry"
      outprog = CurryProg
                  progName (nub $ ["XML",mname] ++ imports) Nothing [] [] []
                  (map (mkType2Xml opts) types ++
                   map (mkXml2Type opts) types ++ specials)
                  []
  writeFile outfile $ nopatwarn ++ showCProg outprog
  putStrLn $ "Now you can import: " ++ outfile
 where
  nopatwarn = "{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}\n\n"

maybeString :: [QName] -> [QName]
maybeString xs
  = if notElem (pre "String") xs && elem (pre "[]") xs && elem (pre "Char") xs
    then (pre "String" : xs)
    else xs

-- Transform original module name into name of the transformation module.
-- Hierarchical module names are "flattened" by replacing dots with underscores.
transModName :: String -> String
transModName mn = map dot2us mn ++ "DataToXml"
 where dot2us c = if c=='.' then '_' else c

----------------------------
-- naming the new functions
----------------------------

toXmlName :: QName -> QName
toXmlName (m,s) = case (isPrelude m, s, isTupleName s) of
  (True,"[]",_)  -> (nm,"list_To_Xml")
  (True,"()",_)  -> (nm,"unitToXml")
  (True,_,True)  -> (nm,"tuple"++show (length s-1)++"ToXml")
  (_,c:cs,_)     -> (nm,toLower c:cs ++ "ToXml")
  (_, [], _)     -> error "Data2Xml.toXmlName: empty identifier"
 where nm = transModName m


fromXmlName :: QName -> QName
fromXmlName (m,s) = case (isPrelude m,s,isTupleName s) of
  (True,"[]",_)  -> (nm,"xml_To_List")
  (True,"()",_)  -> (nm,"xmlToUnit")
  (True,_,True)  -> (nm,"xmlToTuple"++show (length s-1))
  _              -> (nm,"xmlTo"++s)
 where nm = transModName m


listTag :: [Option] -> String
listTag opts = tag opts "List"

isTupleName :: String -> Bool
isTupleName []       = False
isTupleName (n:name) = n == '(' && isTuple name
  where
    isTuple ""  = False
    isTuple [c] = c == ')'
    isTuple (c:c':cs) = c==',' && isTuple (c':cs)

----------------------------
-- generating tags
----------------------------

tag :: [Option] -> String -> String
tag opts s = if LowCase `elem` opts then map toLower s else s

tagNameForCons :: QName -> String
tagNameForCons (mname,cname)
  | isTupleName cname = "Tuple" ++ show (length cname - 1)
  | isPrelude mname   = cname
  | otherwise         = mname++"_"++cname

-------------------------------------------------
-- make functions to transform data terms to xml
-------------------------------------------------

mkType2Xml :: [Option] -> CTypeDecl -> CFuncDecl
mkType2Xml _ (CTypeSyn name vis vars texp) =
  stFunc (toXmlName name) 1 vis
         (CFuncType (applyTC name (map CTVar vars)) xmlType)
         [simpleRule [CPVar (0,"x0")] (call2xml (texp,0))]
mkType2Xml opts (CType name vis vars cs _) =
  stFunc (toXmlName name) (1+length vars) vis
         (type2XmlType vars
                       (CFuncType (applyTC name (map CTVar vars)) xmlType))
         (map (mkConsDecl2Xml opts $ map (CPVar . renVar) vars) cs)
mkType2Xml _ (CNewType _ _ _ _ _) =
  error "Data2Xml.mkType2Xml: CNewType not yet implemented!"

mkConsDecl2Xml :: [Option] -> [CPattern] -> CConsDecl -> CRule
mkConsDecl2Xml opts patVars (CCons name _ args) =
  simpleRule (newPatVars++[CPComb name (pVars arity)])
             (xml opts (tagNameForCons name) []
                  (map call2xml (zip args [0..])))
 where
  arity = length args
  newPatVars = renameUnused (map renVar $ concatMap tvarsOfType args) patVars
mkConsDecl2Xml _ _ (CRecord _ _ _)
  = error "Data2Xml.mkConsDecl2Xml: CRecord not yet implemented!"

type2XmlType :: [(Int,String)] -> CTypeExpr -> CTypeExpr
type2XmlType vars t
  = foldr CFuncType t (map (\x->CFuncType (CTVar x) xmlType) vars)

call2xml :: (CTypeExpr,Int) -> CExpr
call2xml (t,i) = CApply (call2xmlType t) (toVar i)

call2xmlType :: CTypeExpr -> CExpr
call2xmlType (CTVar v) = CVar (renVar v)
call2xmlType (CTCons name) = constF (toXmlName name)
call2xmlType t@(CTApply _ _) =
  maybe (error $ "unable to transform type applications to XML: " ++ show t)
        (\ (name,args) ->
           if name == pre "[]" && args == [charType]
             then constF (toXmlName (pre "String"))
             else applyF (toXmlName name) (map call2xmlType args))
        (tconsArgsOfType t)
call2xmlType t@(CFuncType _ _) =
  error $ "unable to transform function types to XML: " ++ show t

xml :: [Option] -> String -> [CExpr] -> [CExpr] -> CExpr
xml opts name attrs elems
  = applyF ("XML","XElem")
           [string2ac (tag opts name), list2ac attrs, list2ac elems]

xmlType :: CTypeExpr
xmlType = baseType ("XML","XmlExp")

-------------------------------------------------
-- make functions to transform xml to data terms
-------------------------------------------------

mkXml2Type :: [Option] -> CTypeDecl -> CFuncDecl
mkXml2Type _ (CTypeSyn name vis vars texp) =
  stFunc (fromXmlName name) 1 vis
         (CFuncType xmlType (applyTC name (map CTVar vars)))
         [simpleRule [CPVar (0,"x0")] (callXml2 (texp,0))]
mkXml2Type opts (CType name vis vars cs _) =
  stFunc (fromXmlName name) (1+length vars) vis
         (xml2typeType vars
             (CFuncType xmlType (applyTC name (map CTVar vars))))
         (map (mkXml2ConsDecl opts $ map (CPVar . renVar) vars) cs)
mkXml2Type _ (CNewType _ _ _ _ _) =
  error "Data2Xml.mkXml2Type: CNewType not yet implemented!"

renVar :: (a,String) -> (a,String)
renVar (i,s) = case s of
                ('x':xs) -> (i,'t':xs)
                _ -> (i,s)

xml2typeType :: [(Int,String)] -> CTypeExpr -> CTypeExpr
xml2typeType vars t
  = foldr CFuncType t (map (\x->CFuncType xmlType (CTVar x)) vars)

mkXml2ConsDecl :: [Option] -> [CPattern] -> CConsDecl -> CRule
mkXml2ConsDecl opts patVars (CCons name _ args)
  = simpleRule (newPatVars++[pxml opts (tagNameForCons name) [] (pVars arity)])
               (applyF name (map callXml2 (zip args [0..])))
  where
   arity = length args
   newPatVars = renameUnused (map renVar $ concatMap tvarsOfType args) patVars
mkXml2ConsDecl _ _ (CRecord _ _ _)
  = error "Data2Xml.mkXml2ConsDecl: CRecord not yet implemented!"

renameUnused :: [(Int,String)] -> [CPattern] -> [CPattern]
renameUnused _        []     = []
renameUnused usedVars (p:ps) = case p of
  CPVar (i,v) | elem (i,v) usedVars -> CPVar (i,v)   : renameUnused usedVars ps
              | otherwise           -> CPVar (i,"_") : renameUnused usedVars ps
  _                                 -> p             : renameUnused usedVars ps


pxml :: [Option] -> String -> [CPattern] -> [CPattern] -> CPattern
pxml opts name attrs elems
  = CPComb ("XML","XElem")
           [stringPattern (tag opts name), listPattern attrs, listPattern elems]

callXml2 :: (CTypeExpr,Int) -> CExpr
callXml2 (t,i) = CApply (callXml2Type t) (toVar i)

callXml2Type :: CTypeExpr -> CExpr
callXml2Type (CTVar v) = CVar (renVar v)
callXml2Type (CTCons name) = constF (fromXmlName name)
callXml2Type t@(CTApply _ _) =
  maybe (error $ "unable to transform type applications from XML: " ++ show t)
        (\ (name,args) ->
           if name == pre "[]" && args == [charType]
             then constF (fromXmlName (pre "String"))
             else applyF (fromXmlName name) (map callXml2Type args))
        (tconsArgsOfType t)
callXml2Type t@(CFuncType _ _) =
  error $ "unable to transform functions from XML: " ++ show t

-----------------------------
-- treat imported data types
-----------------------------

importTypes :: String -> [QName] -> IO ([String])
importTypes _ ts = do
  let imps = nub (map importType ts)
  imessage imps
  return imps

imessage :: [String] -> IO ()
imessage []        = return ()
imessage [m]       = putStrLn $ "You also need to generate the module " ++ m
imessage (m:m':ms) =
  putStrLn $ "You also need to generate the modules "++(unwords $ m:m':ms)

importType :: QName -> String
importType (m,f)
  | isPrelude m && elem f ["String","[]","Char","Int","Float"]
  = "PreludeDataToXml"
  | isPrelude m && f == "IO"
  = error "unable to transform I/O actions to XML"
  | isPrelude m && f == "Success"
  = error "unable to transform constraints to XML"
  | otherwise = m++"DataToXml"

-----------------------------------------
-- treat special prelude types
-----------------------------------------

specialNames :: [String]
specialNames =
  ["Int","Float","String","Char","IO","Success","[]","()","(,)"
  ,"DET","ShowS","ReadS"]

filterSpecials :: [CTypeDecl] -> [CTypeDecl]
filterSpecials
  = filter ((\ (m,n) -> not (isPrelude m && elem n specialNames)) . typeName)

specialFuncs :: [Option] -> [CFuncDecl]
specialFuncs opts =
  [mkList2xml opts,mkXml2List opts] ++
  concatMap (\tname -> [baseType2xml opts tname, baseTypeXml2 opts tname])
            ["String","Int","Float","Char"] ++
  concatMap (\tdecl -> [mkType2Xml opts tdecl, mkXml2Type opts tdecl])
            (map mkTupleType (0:[2..12]))

-- make tuple type of arity n:
mkTupleType :: Int -> CTypeDecl
mkTupleType n =
  CType (pre tcons) Public tvars
        [simpleCCons (pre tcons) Public (map CTVar tvars)] []
 where tcons = "(" ++ take (n-1) (repeat ',') ++ ")"
       tvars = map (\i -> (i,'a':show i)) [1..n]

mkList2xml :: [Option] -> CFuncDecl
mkList2xml opts =
   stFunc (toXmlName (pre "[]")) 2 Public
     (CFuncType (CFuncType (CTVar (0,"a")) xmlType)
                (CFuncType (listType (CTVar (0,"a"))) xmlType))
     [simpleRule (pVars 2)
                 (applyF ("XML","XElem")
                         [string2ac (listTag opts), list2ac [],
                          applyE (applyF (pre "map") [toVar 0]) [toVar 1]])]

mkXml2List :: [Option] -> CFuncDecl
mkXml2List opts =
   stFunc (fromXmlName (pre "[]")) 2 Public
     (CFuncType (CFuncType xmlType (CTVar (0,"a")))
                (CFuncType xmlType (listType (CTVar (0,"a")))))
     [simpleRule
        [x, CPComb ("XML","XElem") [stringPattern (listTag opts),pNil,y]]
        (applyF (pre "map") [toVar 0,toVar 1])]
  where
    [x,y] = pVars 2

baseType2xml :: [Option] -> String -> CFuncDecl
baseType2xml opts s
  = stFunc (toXmlName (pre s)) 1 Public
     (CFuncType (baseType (pre s)) xmlType)
     [simpleRule (pVars 1) (xml opts s [] [writeFun s])]

baseTypeXml2 :: [Option] -> String -> CFuncDecl
baseTypeXml2 opts s =
  stFunc (fromXmlName (pre s)) 1 Public
   (CFuncType xmlType (baseType (pre s)))
   (simpleRule [pxml opts s [] [CPComb ("XML","XText") (pVars 1)]] (readFun s)
    : if s=="String"
      then [simpleRule [pxml opts s [] []] (string2ac "")]
      else [])

readFun :: String -> CExpr
readFun typ = case typ of
  "Int"    -> applyF ("Prelude","read") [toVar 0]
  "Char"   -> applyF (pre "head") [toVar 0]
  "Float"  -> applyF ("Prelude","read") [toVar 0]
  "String" -> toVar 0
  _        -> error ("Dta2Xml.readFun: unknown type " ++ typ)

writeFun :: String -> CExpr
writeFun s = case s of
  "String" -> applyF ("XML","xtxt") [toVar 0]
  "Char"   -> applyF ("XML","xtxt") [list2ac [toVar 0]]
  _        -> applyF ("XML","xtxt") [applyF (pre "show") [toVar 0]]


---------------------------------------------------------------------
-- Auxiliaries:

--- yield list of all types the given type depends on
requiredTypesTypeDecl :: CTypeDecl -> [QName]
requiredTypesTypeDecl (CTypeSyn _ _ _ e ) = requiredTypesTypeExpr e
requiredTypesTypeDecl (CType    _ _ _ cs _) = concatMap requiredTypesConsDecl cs
requiredTypesTypeDecl (CNewType _ _ _ cd _) = requiredTypesConsDecl cd

requiredTypesConsDecl :: CConsDecl -> [QName]
requiredTypesConsDecl (CCons   _ _ ts) = concatMap requiredTypesTypeExpr ts
requiredTypesConsDecl (CRecord _ _ fs) = concatMap requiredTypesFieldDecl fs
 where requiredTypesFieldDecl (CField _ _ t) = requiredTypesTypeExpr t

requiredTypesTypeExpr :: CTypeExpr -> [QName]
requiredTypesTypeExpr (CTVar _) = []
requiredTypesTypeExpr (CTCons tc) = [tc]
requiredTypesTypeExpr (CTApply tc ta)
  | tc == CTCons (pre "[]") && ta == charType
  = [pre "String"]
  | otherwise
  = requiredTypesTypeExpr tc ++ requiredTypesTypeExpr ta
requiredTypesTypeExpr (CFuncType e1 e2)
  = requiredTypesTypeExpr e1 ++ requiredTypesTypeExpr e2

---------------------------------------------------------------------
