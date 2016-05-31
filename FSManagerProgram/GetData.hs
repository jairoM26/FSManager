{-# OPTIONS_GHC -fno-warn-tabs #-}
{-
 -- @author: Jairo Méndez
 -- @date: 20 - 05 - 2016
 -- @brief: Implementing functions that will getdata from tuples and lists
-}
-----------------------------------------------------------------------------------------------------------------------------------
									-- GETTERS FOR TOUPLE --
-----------------------------------------------------------------------------------------------------------------------------------
module GetData
(tupleGetFirst'
,tupleGetSecond'
,tupleGetThird'
,tupleGetFirst
,tupleGetSecond
,tupleGetThird
,tupleGetFourth
,getListElement
,generateNamesGroupList
,generateAssocUserGroupString
,addspaces
,generateNamesUserList
,generateSecondaryGroupNames
,generatePathStorageDev
,generateVGNames
,generateLVNames
,verifyName
,getStorageSize
,getLVSize
,getVGFreeSize
,getDevice
,getVG
,getLV
,getDir
,getAssocDir
,verifyNumber
,verifyPath
) where 

import Data.Tuple.Select
--Functions for return first, second and third element of a tuple
--pFirst, pSecond and pThird can be any type as they want

{-
 --The functions that have three parameters are used to get the data of the groups
-}
tupleGetFirst' :: (pFirst, pSecond, pThird) -> pFirst  
tupleGetFirst' (x, _, _) = x  
  
tupleGetSecond' :: (pFirst, pSecond, pThird) -> pSecond  
tupleGetSecond' (_, y, _) = y  
  
tupleGetThird' :: (pFirst, pSecond, pThird) -> pThird 
tupleGetThird' (_, _, z) = z  

{-
 --The functions that have four parameters are used to get the data of the users
-}
tupleGetFirst :: (pFirst, pSecond, pThird, pFourth) -> pFirst  
tupleGetFirst (x, _, _,_) = x  
  
tupleGetSecond :: (pFirst, pSecond, pThird, pFourth) -> pSecond  
tupleGetSecond (_, y, _,_) = y  
  
tupleGetThird :: (pFirst, pSecond, pThird, pFourth) -> pThird 
tupleGetThird (_, _, z,_) = z  

tupleGetFourth :: (pFirst, pSecond, pThird, pFourth) -> pFourth
tupleGetFourth (_, _, _,a) = a


										-- FUNCTION GETTER BY INDEX IN A LIST --
-----------------------------------------------------------------------------------------------------------------------------------
--Functions for return an element of a list with an index
getListElement pIndex list = list !! pIndex

-----------------------------------------------------------------------------------------------------------------------------------
										-- FUNCTION FOR GETTING DATA --
-----------------------------------------------------------------------------------------------------------------------------------
generateNamesGroupList :: [(String,Int,[String])]->[String]
generateNamesGroupList [] = [""]
generateNamesGroupList pGroupList =  tupleGetFirst' (head pGroupList) : generateNamesGroupList (tail pGroupList)

generateAssocUserGroupString :: [String]->String
generateAssocUserGroupString [] = ""
generateAssocUserGroupString x =  (head x) ++ ", " ++ (generateAssocUserGroupString (tail x))

addspaces :: (Int,String) -> String
addspaces (size,spaces) = do
     if ( size==0)
          then spaces
     else do
          addspaces(size-1,spaces ++ " ")

generateNamesUserList :: [(String,Int,String,[String])]->[String]
generateNamesUserList [] = []
generateNamesUserList pUserList =  tupleGetFirst (head pUserList) : generateNamesUserList (tail pUserList)

generateSecondaryGroupNames :: [String]->String
generateSecondaryGroupNames [] = ""
generateSecondaryGroupNames x =  (head x) ++ ", " ++ (generateSecondaryGroupNames (tail x))

generatePathStorageDev :: [(String,Int,String,[Bool])]->[String]
generatePathStorageDev [] = []
generatePathStorageDev pStorageList = tupleGetFirst (head pStorageList) : generatePathStorageDev (tail pStorageList)

generateVGNames :: [(String,[String],[String],Int,Int)]->[String]
generateVGNames [] = []
generateVGNames pVGList = sel1 (head pVGList) : generateVGNames (tail pVGList)

generateLVNames :: [(String,Int,Bool)]->[String]
generateLVNames [] = []
generateLVNames pLVList = sel1 (head pLVList) : generateLVNames (tail pLVList)


verifyName :: String -> Bool
verifyName [] = True
verifyName (x:name) 
	| (x `elem` ['0'..'9']) || (x `elem` ['a'..'z']) || (x `elem` ['A'..'Z']) = verifyName name
	| otherwise = False

getStorageSize :: ([(String,Int,String,[Bool])],String) -> Int
getStorageSize ([], storageToFind) = 0
getStorageSize (elem:pStorageList,storageToFind)
	| storageToFind == (sel1 elem) = if ((sel3 elem) == "G") 
										then 1024*(sel2 elem)
										else (sel2 elem)
	| otherwise = getStorageSize (pStorageList,storageToFind)

getVGFreeSize :: ([(String,[String],[String],Int,Int)],String) -> Int
getVGFreeSize ([],vgName) = 0
getVGFreeSize (tmp:pVGList,vgName)
	| vgName == sel1 tmp = sel5 tmp
	|otherwise = getVGFreeSize (pVGList,vgName)

getLVSize :: ([(String,Int,Bool)],String)-> Int
getLVSize ([],lvName) = 0
getLVSize (tmp:lvList,lvName)
	| lvName == sel1 tmp = (sel2 tmp)
	| otherwise = getLVSize (lvList,lvName)
	
getDevice :: ([(String,Int,String,[Bool])],String) -> (String,Int,String,[Bool])
getDevice (tmp:storageList,dev)
	| dev == (sel1 tmp) = tmp
	| otherwise = getDevice (storageList,dev)

getVG :: ([(String,[String],[String],Int,Int)],String) -> (String,[String],[String],Int,Int)
getVG (tmp:pVGList,vg)
	| vg == (sel1 tmp) = tmp
	| otherwise = getVG (pVGList,vg)

getLV ::([(String,Int,Bool)],String) -> (String,Int,Bool)
getLV (tmp:pLVList,lv) 
	| lv == sel1 (tmp) = tmp
	|otherwise = getLV(pLVList,lv)

getDir :: ([(String,String,[String],[String],String,String,String,String)],String)->(String,String,[String],[String],String,String,String,String)
getDir (tmp:directoryList,dirName)
	| dirName == sel1 tmp = tmp
	| otherwise = getDir (directoryList,dirName)


getAssocDir :: ([(String,String,[String],[String],String,String,String,String)],String)->[String]
getAssocDir ([],dirName) = []
getAssocDir (tmpDir:directoryList, dirName)
	| dirName == sel1 tmpDir = sel3 tmpDir
	| otherwise = getAssocDir (directoryList,dirName)

verifyNumber :: String -> Bool
verifyNumber [] = True
verifyNumber (x:numberList)
	| ( x `elem` ['0'..'9'] )= verifyNumber numberList
	| otherwise = False

verifyPath :: String -> Bool
verifyPath pathList = if (take 5 pathList) == "/dev/"
						then True
						else False