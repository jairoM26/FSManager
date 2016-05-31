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
,updateDirectories
,createDirectory
,verifyRootPath
,findDir
,findDirByPath
,findFile
,getDirByPath
,generatePathOfAllDir
,updateDirectoriesFiles
,createFile
,getDirCD
,findFileInDir
,getStringBySplit
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


verifyNumber :: String -> Bool
verifyNumber [] = True
verifyNumber (x:numberList)
	| ( x `elem` ['0'..'9'] )= verifyNumber numberList
	| otherwise = False

verifyPath :: String -> Bool
verifyPath pathList = if (take 5 pathList) == "/dev/"
						then True
						else False


---------------MKDIR FUNCTIONS---------------------
updateDirectories :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,[String],[String],String,String,String,String,String,Bool)],String,String,String,String) -> [(String,String,[String],[String],String,String,String,String,String,Bool)]
updateDirectories ([],tmpDirList,dirNameToUpdate,dirNameToAdd,date,time) = tmpDirList
updateDirectories (tmpDir:directoriesList,tmpDirList,dirNameToUpdate,dirNameToAdd,date,time)
	| dirNameToUpdate == sel1 tmpDir = updateDirectories (directoriesList,tmpDirList ++ [(dirNameToUpdate,sel2 tmpDir,sel3 tmpDir ++ [dirNameToAdd],sel4 tmpDir,date,time,sel7 tmpDir,sel8 tmpDir,sel9 tmpDir,sel10 tmpDir)],dirNameToUpdate,dirNameToAdd,date,time)
	| otherwise = updateDirectories (directoriesList,tmpDirList ++ [tmpDir], dirNameToUpdate,dirNameToAdd,date,time)


updateDirectoriesFiles :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,[String],[String],String,String,String,String,String,Bool)],String,String,String,String) -> [(String,String,[String],[String],String,String,String,String,String,Bool)]
updateDirectoriesFiles ([],tmpDirList,dirNameToUpdate,fileNameToAdd,date,time) = tmpDirList
updateDirectoriesFiles (tmpDir:directoriesList,tmpDirList,dirNameToUpdate,fileNameToAdd,date,time)
	| dirNameToUpdate == sel2 tmpDir = updateDirectoriesFiles (directoriesList,tmpDirList ++ [(sel1 tmpDir,sel2 tmpDir,sel3 tmpDir,sel4 tmpDir ++ [fileNameToAdd],date,time,sel7 tmpDir,sel8 tmpDir,sel9 tmpDir,sel10 tmpDir)],dirNameToUpdate,fileNameToAdd,date,time)
	| otherwise = updateDirectoriesFiles (directoriesList,tmpDirList ++ [tmpDir], dirNameToUpdate,fileNameToAdd,date,time)


createDirectory :: (String,String,String,String) -> (String,String,[String],[String],String,String,String,String,String,Bool)
createDirectory (dirName,dirpath,date,time) = (dirName, dirpath ,[], [], date, time,"root:root","/","",False)

createFile :: (String,String,String,String) -> (String,String,String,String,String,String)
createFile (fileName,filePath,date,time) = (fileName,filePath, "",date,time,"root:root")


verifyRootPath :: (String) -> Bool
verifyRootPath ([]) = False
verifyRootPath (dirpath)
	| dirpath !! 0 == '/' = True
	| otherwise = False



findDir :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String) -> Bool
findDir ([],dirName) = False
findDir (tmp:directoriesList,dirName)
	| dirName == sel1 tmp = True
	| otherwise = findDir (directoriesList,dirName)

findFile :: ([(String,String,String,String,String,String)],String)-> Bool
findFile ([], fileName) = False
findFile (tmp:fileList,fileName)
	| fileName == sel1 tmp = True
	| otherwise = findFile(fileList,fileName)


findDirByPath :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String) -> Bool
findDirByPath ([],dirName) = False
findDirByPath (tmp:directoriesList,dirName)
	| dirName == sel2 tmp = True
	| otherwise = findDirByPath (directoriesList,dirName)

findFileInDir :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String,String)-> Bool
findFileInDir ([],dirName,fileName) = False
findFileInDir (tmp:directoriesList,dirName,fileName)
	| dirName == sel1 tmp = if (fileName `elem` (sel4 tmp))
								then True
								else False
	| otherwise = findFileInDir (directoriesList,dirName,fileName)

generatePathOfAllDir :: ([(String,String,[String],[String],String,String,String,String,String,Bool)])->[String]
generatePathOfAllDir ([]) = []
generatePathOfAllDir (tmp:directoryList) = (sel2 tmp) : generatePathOfAllDir (directoryList)

getDirCD :: ([(String,String,[String],[String],String,String,String,String,String,Bool)])->String
getDirCD directoryList = sel8 (head directoryList)

getDir :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String)->(String,String,[String],[String],String,String,String,String,String,Bool)
getDir (tmp:directoryList,dirName)
	| dirName == sel1 tmp = tmp
	| otherwise = getDir (directoryList,dirName)

getDirByPath :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String)->(String,String,[String],[String],String,String,String,String,String,Bool)
getDirByPath (tmp:directoryList,dirName)
	| dirName == sel2 tmp = tmp
	| otherwise = getDirByPath (directoryList,dirName)



getAssocDir :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String)->[String]
getAssocDir ([],dirName) = []
getAssocDir (tmpDir:directoryList, dirName)
	| dirName == sel1 tmpDir = sel3 tmpDir
	| otherwise = getAssocDir (directoryList,dirName)


getStringBySplit :: ([String])->String
getStringBySplit ([]) = ""
getStringBySplit(tmp:splitList) = ("/" ++ tmp) ++ getStringBySplit (splitList)
	