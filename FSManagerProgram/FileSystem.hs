{-# OPTIONS_GHC -fno-warn-tabs #-}
module FileSystem 
(mkfs
,mkdir
)
where 
import GetData
import VolumeGroups
import Data.Tuple.Select
import Data.List.Split-- library split
import CreateStorageDevice

---------------------------------------------------MKFS FUNCTIONS-------------------------------------------------------------------------
{-
-}
mkfs :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't make file system, expected: -t type path")
mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| x == "-t" = mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| (x == "ext2" || x == "ext3" || x == "ext4") && ((length xs) == 1) = mkfsAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x, head xs)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't find: " ++ x)

{-
-}
mkfsAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
mkfsAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,pfsType, fpath)
	{-
		--Verify if the paths is ok, and if it is a logical volume path or storage device path
		-- path = /dev/vgName/lvName
		-- path = /dev/storageDeviceName
	-} 
	| verifyPath (fpath) = if findVG (pVGList, head (tail (tail(splitOn  "/" fpath)))) && findVL(pLVList,last (tail (tail(splitOn  "/" fpath)))) --if its a vg/lv
							then if (sel3 (getLV (pLVList, last (tail (tail(splitOn  "/" fpath)))))) --verify if the lv is already mounted as file system
								then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't make file system, the logical volume is already mountes as file system")
								else createFS(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,pfsType,fpath,'0')
							-- the path is not from a logical volume, so it should be a storage device path, or the path doesn't exists
							else if (findStorage (pStorageList,fpath)) || (last (sel4 (getDevice (pStorageList,fpath)))) || not (head (sel4 (getDevice (pStorageList,fpath)))) --verify if the storage device is already mounted
									then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't make file system, the storage device is already mountes as file system")
									else createFS(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,pfsType,fpath,'1')
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't find: " ++ fpath ++ " make a file system, the path should be /dev/vgName/lvName, or /dev/storageDeviceName")

{-
-}
createFS ::([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],String,String,Char)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
createFS (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,pfsType,fpath,pathType) 
	| pathType == '1' = (pGroupList,pUserList,(updateStorageDevices (pStorageList,fpath),pVGList,pLVList),(pFileSystList ++ [(fpath,pfsType,pathType,[], getStorageSize(pStorageList,fpath),"")],directoriesList,filesList),"")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList, updateLV (pLVList,last (tail (tail(splitOn  "/" fpath))))),(pFileSystList ++ [(fpath,pfsType,pathType,[], getLVSize(pLVList,last (tail (tail(splitOn  "/" fpath)))),"")],directoriesList,filesList),"" )

{-
-}
updateStorageDevices (tmp:pStorageList,devName)
	| devName == sel1 tmp = (devName,sel2 tmp, sel3 tmp, [head (sel4 (tmp)),True]) : updateStorageDevices (pStorageList,devName)
	| otherwise = tmp : updateStorageDevices (pStorageList,devName)

{-
-}
updateLV :: ([(String,Int,Bool)],String)->[(String,Int,Bool)]
updateLV ([],lvName) = []
updateLV (tmp:pLVList,lvName)
	| lvName == sel1 tmp = (lvName, sel2 tmp, True) : updateLV (pLVList,lvName)
	| otherwise = tmp : updateLV (pLVList,lvName)
---------------------------------------------------MKFS FUNCTIONS ENDS-------------------------------------------------------------------------

---------------------------------------------------MKDIR FUNCTIONS-----------------------------------------------------------------------------
mkdir :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
mkdir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do mkdir, expected /path or -p /path")
mkdir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = mkdir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| x == "-p" = mkdirP(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	--in call mkdirAux, x = /carp1/carp2 ... if carp1 exists so generate carp2 in carp1, if carp1 does not exists so an error will be printed
	--if x = /carp1/carp2, so tail (splitOn  "/" x) return a list that looks like this (carp1,carp2)	
	-- "" will be used to represent the last directory accesed
	| verifyRootPath (x) && (null xs) = mkdirAux(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,tail (splitOn  "/" x),"/",x)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't find "++ x)

mkdirAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],[String],String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
mkdirAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,dirName:folderList,lastDir,dirpath)
	{-
	asume folderName = /carp1/carp2/carp3, and then dirName = carp3 and also carp1 and carp2 already exists so you can create a directory
	if carp3 already exists so it sends a message
	-}
	| (length folderList) == 0 && not (dirName `elem` (getAssocDir (directoriesList,lastDir))) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName)) ++ [createDirectory (dirName,dirpath)],filesList),"")
	| (length folderList) == 0 && (dirName `elem` (getAssocDir (directoriesList,lastDir))) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The directory " ++ dirName ++ " is already created in " ++ dirpath)
	{-
	asume folderName = /carp1/carp2/carp3, and then dirName = carp2 you have to check if that directory exists so if its exist
	-}
	| not (findDir(directoriesList,lastDir)) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The directory " ++ dirName ++ " isn't created yet")	
	| otherwise = mkdirAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,folderList,dirName,dirpath)

mkdirP :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
mkdirP (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do mkdir, expected /path")
mkdirP (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| verifyRootPath (x) && (null xs) = mkdirPAux(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,tail (splitOn  "/" x),"/","/",x)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't find "++ x)


mkdirPAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],[String],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
mkdirPAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,dirName:folderList,lastDir,lastLastDir,dirpath)
	{-
	asume folderName = /carp1/carp2/carp3, and then dirName = carp3 and also carp1 and carp2 already exists so you can create a directory
	if carp3 already exists so it sends a message
	-}
	| (length folderList) == 0 && not (dirName `elem` (getAssocDir (directoriesList,lastDir))) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName)) ++ [createDirectory (dirName,dirpath)],filesList),"")
	| (length folderList) == 0 && (dirName `elem` (getAssocDir (directoriesList,lastDir))) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The directory " ++ dirName ++ " is already created in " ++ dirpath)
	{-
	asume folderName = /carp1/carp2/carp3, and then dirName = carp2 you have to check if that directory exists so if its exist
	-}
	| not (findDir(directoriesList,dirName)) = 	mkdirPAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName)) ++ [createDirectory (dirName,dirpath)],filesList,folderList,dirName,lastDir,dirpath)
	| otherwise = mkdirPAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,folderList,dirName,lastDir,dirpath)


updateDirectories :: ([(String,String,[String],[String],String,String,String,String)],[(String,String,[String],[String],String,String,String,String)],String,String) -> [(String,String,[String],[String],String,String,String,String)]
updateDirectories ([],tmpDirList,dirNameToUpdate,dirNameToAdd) = tmpDirList
updateDirectories (tmpDir:directoriesList,tmpDirList,dirNameToUpdate,dirNameToAdd)
	| dirNameToUpdate == sel1 tmpDir = updateDirectories (directoriesList,tmpDirList ++ [(dirNameToUpdate,sel2 tmpDir,sel3 tmpDir ++ [dirNameToAdd],sel4 tmpDir,sel5 tmpDir,sel6 tmpDir,sel7 tmpDir,sel8 tmpDir)],dirNameToUpdate,dirNameToAdd)
	| otherwise = updateDirectories (directoriesList,tmpDirList ++ [tmpDir], dirNameToUpdate,dirNameToAdd)

createDirectory :: (String,String) -> (String,String,[String],[String],String,String,String,String)
createDirectory (dirName,dirpath) = (dirName, dirpath ,[], [], "", "","root:root","")


verifyRootPath :: (String) -> Bool
verifyRootPath (tmp:dirpath)
	| tmp== '/' = True
	| otherwise = False



findDir :: ([(String,String,[String],[String],String,String,String,String)],String) -> Bool
findDir ([],dirName) = False
findDir (tmp:directoriesList,dirName)
	| dirName == sel1 tmp = True
	| otherwise = findDir (directoriesList,dirName)







