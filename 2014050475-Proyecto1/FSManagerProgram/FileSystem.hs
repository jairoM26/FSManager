{-# OPTIONS_GHC -fno-warn-tabs #-}
module FileSystem 
(mkfs
,mkdir
,cd
,touch
,echo
,rmdir
,ls
,chown
,printDir
,printAssocDir
)
where 
import GetData
import VolumeGroups
import AddGroups
import AddUsers
import Data.Tuple.Select
import Data.List.Split-- library split
import CreateStorageDevice

---------------------------------------------------MKFS FUNCTIONS-------------------------------------------------------------------------
{-
-}
mkfs :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't make file system, expected: -t type path")
mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| x == "-t" = mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| (x == "ext2" || x == "ext3" || x == "ext4") && ((length xs) == 1) = mkfsAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x, head xs)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't find: " ++ x)

{-
-}
mkfsAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
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
							else if (findStorage (pStorageList,fpath)) 
								then if not (last (sel4 (getDevice (pStorageList,fpath)))) && not (head (sel4 (getDevice (pStorageList,fpath)))) --verify if the storage device is already mounted
									then createFS (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,pfsType,fpath,'1') 
									else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't make file system, the storage device is already mounted as file system")
								else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't make file system, the storage device doesn't exists")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't find: " ++ fpath ++ " make a file system, the path should be /dev/vgName/lvName, or /dev/storageDeviceName")

{-
-}
createFS ::([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String,String,Char)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
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
mkdir :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
mkdir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y,date,time) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do mkdir, expected /path or -p /path")
mkdir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y,date,time)
	| x == "" = mkdir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y,date,time)
	| x == "-p" = mkdirP(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y,date,time)
	--in call mkdirAux, x = /carp1/carp2 ... if carp1 exists so generate carp2 in carp1, if carp1 does not exists so an error will be printed
	--if x = /carp1/carp2, so tail (splitOn  "/" x) return a list that looks like this (carp1,carp2)	
	-- "" will be used to represent the last directory accesed
	| verifyRootPath(x) && (null xs) = mkdirAux(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,tail (splitOn  "/" x),"/",x,date,time)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't find "++ x)

mkdirAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[String],String,String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
mkdirAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,dirName:folderList,lastDir,dirpath,date,time)	
	{-																
	asume folderName = /carp1/carp2/carp3, and then dirName = carp3 and also carp1 and carp2 already exists so you can create a directory
	if carp3 already exists so it sends a message
	-}
	| (length folderList) == 0 && not (dirName `elem` (getAssocDir (directoriesList,lastDir))) = if getDirCD(directoriesList) == "/" 
																									then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName,date,time)) ++ [createDirectory (dirName, dirpath,date,time)],filesList),"")
																									else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,(updateDirectories(directoriesList,[],sel1 (getDirByPath(directoriesList,(getDirCD(directoriesList)))),dirName,date,time)) ++ [createDirectory (dirName,getDirCD(directoriesList) ++ dirpath,date,time)],filesList),"")
	| (length folderList) == 0 && (dirName `elem` (getAssocDir (directoriesList,lastDir))) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The directory " ++ dirName ++ " is already created in " ++ dirpath)
	{-
	asume folderName = /carp1/carp2/carp3, and then dirName = carp2 you have to check if that directory exists so if its exist
	-}
	| not (findDir(directoriesList,dirName)) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The directory " ++ dirpath ++ " isn't created yet")	
	| otherwise = mkdirAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,folderList,dirName,dirpath,date,time)


mkdirP :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
mkdirP (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y,date,time) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do mkdir, expected /path")
mkdirP (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y,date,time)
	| verifyRootPath (x) && (null xs) = mkdirPAux(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,tail (splitOn  "/" x),"/","/","",x,date,time)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't find "++ x)


mkdirPAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[String],String,String,String,String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
mkdirPAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,dirName:folderList,lastDir,lastLastDir,dirpathAux,dirpath,date,time)
	{-
	asume folderName = /carp1/carp2/carp3, and then dirName = carp3 and also carp1 and carp2 already exists so you can create a directory
	if carp3 already exists so it sends a message
	-}
	| (length folderList) == 0 && not (dirName `elem` (getAssocDir (directoriesList,lastDir))) = if getDirCD(directoriesList) == "/" 
																									then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName,date,time)) ++ [createDirectory (dirName, dirpath,date,time)],filesList),"")
																									else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName,date,time)) ++ [createDirectory (dirName,getDirCD(directoriesList) ++ dirpath,date,time)],filesList),"")
	| (length folderList) == 0 && (dirName `elem` (getAssocDir (directoriesList,lastDir))) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The directory " ++ dirName ++ " is already created in " ++ dirpath)
	{-
	asume folderName = /carp1/carp2/carp3, and then dirName = carp2 you have to check if that directory exists so if its exist
	-}
	| not (findDir(directoriesList,dirName)) = if getDirCD(directoriesList) == "/" 
												then if dirpathAux =="" 
														then	mkdirPAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName,date,time)) ++ [createDirectory (dirName,dirpathAux++"/"++dirName,date,time)],filesList,folderList,dirName,lastDir,dirpathAux++"/"++dirName,dirpath,date,time)
														else 	mkdirPAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName,date,time)) ++ [createDirectory (dirName,dirpathAux++"/"++dirName,date,time)],filesList,folderList,dirName,lastDir,dirpathAux++"/"++dirName,dirpath,date,time)	
												else if dirpathAux =="" 	
														then	mkdirPAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName,date,time)) ++ [createDirectory (dirName,getDirCD(directoriesList) ++dirpathAux++"/"++dirName,date,time)],filesList,folderList,dirName,lastDir,dirpathAux++"/"++dirName,dirpath,date,time)
														else 	mkdirPAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,(updateDirectories(directoriesList,[],lastDir,dirName,date,time)) ++ [createDirectory (dirName,getDirCD(directoriesList) ++dirpathAux++"/"++dirName,date,time)],filesList,folderList,dirName,lastDir,dirpathAux++"/"++dirName,dirpath,date,time)													
	| otherwise = mkdirPAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,folderList,dirName,lastDir,dirpathAux++"/"++dirName,dirpath,date,time)



---------------------------------------------------MKDIR FUNCTIONS ENDS-----------------------------------------------------------------------------

---------------------------------------------------CD FUNCTIONS---------------------------------------------------------------------------------
cd :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
cd (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do cd, expected /path")
cd (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = cd (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| x == "/" && null (xs)= (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirectoriesCD(directoriesList,x),filesList),"")
	| getDirCD(directoriesList) == "/"  = if (x`elem` (generatePathOfAllDir (directoriesList)))
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirectoriesCD(directoriesList,x),filesList),"")
												else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do cd, " ++ x ++ "does not exist")
	| (null xs) && ((getDirCD(directoriesList)++ x)`elem` (generatePathOfAllDir (directoriesList))) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirectoriesCD(directoriesList,getDirCD(directoriesList) ++ x),filesList),"")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do cd, " ++ x ++ "does not exist")


updateDirectoriesCD :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String)->([(String,String,[String],[String],String,String,String,String,String,Bool)])
updateDirectoriesCD (tmp:directoriesList,cd) 
	| null directoriesList = [(sel1 tmp, sel2 tmp, sel3 tmp, sel4 tmp, sel5 tmp, sel6 tmp, sel7 tmp,cd,sel9 tmp, sel10 tmp)]
	| otherwise = (sel1 tmp, sel2 tmp, sel3 tmp, sel4 tmp, sel5 tmp, sel6 tmp, sel7 tmp,cd,sel9 tmp, sel10 tmp) : updateDirectoriesCD (directoriesList,cd)
---------------------------------------------------CD FUNCTIONS ENDS---------------------------------------------------------------------------------

---------------------------------------------------TOUCH FUNCTIONS---------------------------------------------------------------------------------
touch :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
touch (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y,date,time) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do touch, expected /path/filename or filename")
touch (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y,date,time)
	| x == "" = touch (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y,date,time)
	| verifyRootPath(x) && getDirCD(directoriesList) == "/" = if findDirByPath (directoriesList,getStringBySplit(init (tail (splitOn "/" x))))
																then if findFile(filesList,last (tail (splitOn "/" x)))
																		then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do touch, " ++ last (tail (splitOn "/" x)) ++ " file already exists")
																		else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirectoriesFiles (directoriesList,[],getStringBySplit(init (tail (splitOn "/" x))),last (tail (splitOn "/" x)),date,time),filesList ++ [createFile (last (tail (splitOn "/" x)),getStringBySplit(init (tail (splitOn "/" x)))++ "/" ++ (last (tail (splitOn "/" x))),date,time)]),"")
																else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do touch, " ++ getStringBySplit(init (tail (splitOn "/" x))) ++ " doesn't exist")
	| not (verifyRootPath(x)) && getDirCD(directoriesList) /= "/"	= if findFile(filesList,x)
																		then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't do touch, " ++ x ++ " file already exists")
																		else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirectoriesFiles(directoriesList, [], getDirCD(directoriesList),x,date,time),filesList ++ [createFile (x, getDirCD(directoriesList)++"/" ++ x,date,time)]),"")
	| getDirCD(directoriesList) == "/"	&& not (verifyRootPath(x)) = if findFile(filesList,x)
																		then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't do touch, " ++ x ++ " file already exists")
																		else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirectoriesFiles(directoriesList, [], getDirCD(directoriesList),x,date,time),filesList ++ [createFile (x, getDirCD(directoriesList) ++ x,date,time)]),"")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do touch " ++ getDirCD(directoriesList) ++ "  "  ++ getStringBySplit(init (tail (splitOn "/" x))))
---------------------------------------------------TOUCH FUNCTIONS ENDS---------------------------------------------------------------------------------	

---------------------------------------------------ECHO FUNCTIONS----------------------------------------------------------------------------------------
echo :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
echo (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y,date,time) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do echo, expected: filedata >> /path/filename or filename")
echo (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y,date,time)
	| x == "" = echo (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y,date,time)
	| x == ">>" = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do echo, expected: filedata >> /path/filename or filename")
	| (length xs) == 2 = echoAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,x,date,time)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do echo, expected: filedata >> /path/filename or filename")

echoAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
echoAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],fileData,date,time) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do echo, expected: filedata >> /path/filename or filename")
echoAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,fileData,date,time)
	| x == "" = echoAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,fileData,date,time)
	| x == ">>" && (length xs) == 1 = echoAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,fileData,date,time)
	| length xs == 0 = modifyFile (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,fileData,x,date,time)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't find "++ x)

modifyFile :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String,String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
modifyFile (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,fileData,filePath,date,time)
	| verifyRootPath(filePath) && getDirCD(directoriesList) == "/" = if findDirByPath (directoriesList,getStringBySplit(init (tail (splitOn "/" filePath))))
																then if findFile(filesList,last (tail (splitOn "/" filePath)))
																		then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList, modifyFileByName(filesList,[], last (tail (splitOn "/" filePath)),fileData,date,time)),"")
																		else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't do echo, the file " ++ last (tail (splitOn "/" filePath)) ++ " doesn't exist")
																else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do echo, " ++ getStringBySplit(init (tail (splitOn "/" filePath))) ++ " doesn't exist")
	| not (verifyRootPath(filePath)) && getDirCD(directoriesList) /= "/"= if findFile(filesList,filePath)
																			then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList, modifyFileByName(filesList,[], filePath,fileData,date,time)),"")
																			else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't do echo, the file " ++ filePath ++ " doesn't exist")
	| getDirCD(directoriesList) == "/"	&& not (verifyRootPath(filePath)) = if findFile(filesList,filePath)
																			then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList, modifyFileByName(filesList,[], filePath,fileData,date,time)),"")
																			else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't do echo, the file " ++ filePath ++ " doesn't exist")
	{-| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do touch " ++ getDirCD(directoriesList) ++ "  "  ++ getStringBySplit(init (tail (splitOn "/" filePath))))-}


modifyFileByName :: ([(String,String,String,String,String,String)],[(String,String,String,String,String,String)],String,String,String,String)-> [(String,String,String,String,String,String)]
modifyFileByName ([],tmpFilesList,filePath,fileData,date,time) = tmpFilesList
modifyFileByName (tmpFile:filesList,tmpFilesList,fileName,fileData,date,time)
	| fileName == sel1 tmpFile = modifyFileByName(filesList,tmpFilesList ++ [(fileName,sel2 tmpFile, (sel3 tmpFile) ++ fileData, date,time,sel6 tmpFile)],fileName,fileData,date,time)
	| otherwise = modifyFileByName (filesList,tmpFilesList ++ [tmpFile],fileName,fileData,date,time)
---------------------------------------------------ECHO FUNCTIONS ENDS----------------------------------------------------------------------------------------

---------------------------------------------------RM FUNCTIONS----------------------------------------------------------------------------------------
rmdir :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
rmdir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't rm, expected: /directoryPath )")
rmdir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = rmdir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)	
	| (length xs) == 0 = if x /= getDirCD(directoriesList) 
							then delete(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x)
							else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't rm, directory path " ++  x ++ " is in cd at this moment")


delete :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
delete (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,dirPath)
	| getDirCD(directoriesList) == "/" = if findDirByPath (directoriesList,dirPath)
											then if sel10 (getDirByPath(directoriesList,dirPath))
										 	    then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't remove, directory path " ++ dirPath ++ " its beaing used as a file system mount point")
											  	else deleteEmptyDir(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,dirPath)
											else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't rm, directory path " ++ dirPath ++ " doesn't exists")
	| otherwise = if findDirByPath (directoriesList,getDirCD(directoriesList) ++ dirPath)
											then if sel10 (getDirByPath(directoriesList,getDirCD(directoriesList) ++ dirPath))
										 	    then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't remove, directory path "++ getDirCD(directoriesList) ++ dirPath ++ " its beaing used as a file system mount point")
											  	else deleteEmptyDir(pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,getDirCD(directoriesList) ++ dirPath)
											else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't rm, directory path " ++ getDirCD(directoriesList) ++ dirPath ++ " doesn't exists")

deleteEmptyDir :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
deleteEmptyDir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,dirPath)
	| (length(sel3 (getDirByPath(directoriesList,dirPath)))) == 0 && (length (sel4 (getDirByPath(directoriesList,dirPath)))) == 0 = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,removeEmptyDir (updateRemoveDirectories(directoriesList,getDirCD(directoriesList), sel1 (getDirByPath(directoriesList,dirPath))),dirPath),filesList),"")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't rm, directory path " ++ dirPath ++ " have directories or files inside, if you wnat to deleted use rm -rf " ++ dirPath)

removeEmptyDir :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String)->[(String,String,[String],[String],String,String,String,String,String,Bool)]
removeEmptyDir ([],dirToDelete) = []
removeEmptyDir (tmp:directoriesList,dirToDelete)
	| dirToDelete == sel2 tmp = removeEmptyDir(directoriesList,dirToDelete)
	| otherwise = tmp : removeEmptyDir(directoriesList,dirToDelete)

updateRemoveDirectories :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String,String)->[(String,String,[String],[String],String,String,String,String,String,Bool)]
updateRemoveDirectories ([],dirToUpdate,dirToDelete) = []
updateRemoveDirectories (tmpDir:directoriesList,dirToUpdate,dirToDelete)
	| dirToUpdate == sel2 tmpDir = if (dirToDelete `elem` (sel3 tmpDir))
									then (sel1 tmpDir, sel2 tmpDir, deleteAssocDir (sel3 tmpDir,dirToDelete), sel4 tmpDir, sel5 tmpDir, sel6 tmpDir, sel7 tmpDir, sel8 tmpDir,sel9 tmpDir,sel10 tmpDir) : updateRemoveDirectories (directoriesList,dirToUpdate,dirToDelete)
									else tmpDir : updateRemoveDirectories (directoriesList,dirToUpdate,dirToDelete)
	| otherwise = tmpDir : updateRemoveDirectories (directoriesList,dirToUpdate,dirToDelete)

deleteAssocDir :: ([String],String)->[String]
deleteAssocDir ([],dirToDelete) = []
deleteAssocDir (tmp:assocDirList,dirToDelete)
	| dirToDelete == tmp = "hola" :deleteAssocDir (assocDirList,dirToDelete)
	| otherwise = tmp : deleteAssocDir (assocDirList,dirToDelete)


rm :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
rm (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't rm, expected: -rf /directoryPath or /filePath")
rm (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = rm (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| x == "-rf" && (length xs) == 1 = rmRfDir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	{-| null xs =  -}



rmRfDir :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
rmRfDir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't rm, expected: -rf /directoryPath")
rmRfDir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = rmRfDir (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)

	| verifyRootPath(x) && getDirCD(directoriesList) == "/" = if findDirByPath (directoriesList,getStringBySplit((tail (splitOn "/" x))))
																	then if sel10 (getDirByPath(directoriesList,getStringBySplit((tail (splitOn "/" x)))))
																		then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't rm, directory path" ++ x ++ "its beaing used as a file system mount point")
																		else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),x ++ " exists HOLA")
																		{-else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList, deleteDir(deleteAssocDir (directoriesList,sel3 getDir(dirToDelete)),[],getDirCD(directoriesList),getStringBySplit((tail (splitOn "/" x)))))++[sel1 (getDirByPath(directoriesList,getStringBySplit((tail (splitOn "/" x)))))]), deleteFiles(filesList, sel4 (getDirByPath(directoriesList,getStringBySplit((tail (splitOn "/" x))))))),"")-}
																	else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't rm, directory path " ++ x ++ " doesn't exists")
	
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do touch " ++ getDirCD(directoriesList) ++ "  "  ++ getStringBySplit(init (tail (splitOn "/" x))))
	{-															
	| not (verifyRootPath(x)) && getDirCD(directoriesList) /= "/" =()

	| getDirCD(directoriesList) == "/"	&& not (verifyRootPath(x)) = 
	-}
	
{-}
deleteDir :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,[String],[String],String,String,String,String,String,Bool)],String,String)->[(String,String,[String],[String],String,String,String,String,String,Bool)]
deleteDir ([],tmpDirList,dirToUpdate,dirToDelete) = tmpDirList
deleteDir (tmpDir:directoriesList,tmpDirList,dirToUpdate,dirToDelete)
	| dirToUpdate == sel 1tmpDir = 

deleteAssocDir :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],[String])->[(String,String,[String],[String],String,String,String,String,String,Bool)]
deleteAssocDir (directoriesList,[]) = []
deleteAssocDir (directoriesList,tmp:listAssocDir) = deleteAssocDirAux (directoriesList,[] ,tmp)

deleteAssocDirAux :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[String])->[(String,String,[String],[String],String,String,String,String,String,Bool)]
deleteAssocDirAux ([],tmpDirList,dirToDelete) = tmpDirList
deleteAssocDirAux (tmpDir:directoriesList,tmpDirList,dirToDelete)
	| dirToDelete == sel1 tmpDir = deleteAssocDirAux (directoriesList,tmpDirList,dirToDelete)
	| otherwise = tmpDir : deleteAssocDirAux (tmpDir:directoriesList,tmpDirList,dirToDelete)
-}
----------------------------------------------------RM FUNCTIONS ENDS-------------------------------------------------------------------

----------------------------------------------------LS FUNCTIONS -----------------------------------------------------------------------
ls ::([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
ls (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printDir(getDirByPath(directoriesList,getDirCD(directoriesList))))
ls (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = ls (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| x == "-l" && (length xs) /= 0= lsAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| x == "-ld" && (length xs) /= 0= ld (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| (getDirCD(directoriesList) == "/") = if (findDirByPath(directoriesList,x))								
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printDir(getDirByPath(directoriesList,x)))
												else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ x ++ " doesn't exists")
	| (getDirCD(directoriesList) /= "/") = if (findDirByPath(directoriesList,getDirCD(directoriesList) ++ x))
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printDir(getDirByPath(directoriesList,x)))
												else if findDir(directoriesList,x)
													then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printDir(getDir(directoriesList,x)))
													else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ x ++ " doesn't exists")

ld :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
ld (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do ls -ld, expected directoryPath or filePath")
ld (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = ld (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| (getDirCD(directoriesList) == "/") = if (findDirByPath(directoriesList,x))								
											then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printDirData(getDirByPath(directoriesList,x)))
											else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ x ++ " doesn't exists")
	| (getDirCD(directoriesList) /= "/") = if (findDirByPath(directoriesList,getDirCD(directoriesList) ++ x))
											then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printDirData(getDirByPath(directoriesList,x)))
											else if findDir(directoriesList,x)
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printDirData(getDir(directoriesList,x)))
												else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ x ++ " doesn't exists")

lsAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
lsAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do ls -l, expected directoryPath or filePath")
lsAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = lsAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| (getDirCD(directoriesList) == "/") = if (findDirByPath(directoriesList,x))								
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printSpecificDirData(directoriesList,filesList,getDirByPath(directoriesList,x)))
												else if (findFileByPath(filesList,x))								
													then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printSpecificFileData(getFileByPath(filesList,x)))
													else if (findFile(filesList,x))
														then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printSpecificFileData(getFile(filesList,x)))
														else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ x ++ " doesn't exists")
	| (getDirCD(directoriesList) /= "/") = if (findDirByPath(directoriesList,getDirCD(directoriesList) ++ x))
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printSpecificDirData(directoriesList,filesList,getDirByPath(directoriesList,getDirCD(directoriesList) ++x)))
												else if findDir(directoriesList,x)
													then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printSpecificDirData(directoriesList,filesList,getDir(directoriesList,x)))
													else if (findFileByPath(filesList,getDirCD(directoriesList) ++ x))
														then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printSpecificFileData(getFileByPath(filesList,x)))
														else if (findFile(filesList,x))
															then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),printSpecificFileData(getFile(filesList,x)))
															else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ x ++ " doesn't exists")

---------------------------------------get data information form directories and files -------------------------------------------
printDir :: (String,String,[String],[String],String,String,String,String,String,Bool)->String
printDir (dirName,dirPath,assocDir,assocFiles,date,time,ownership,cdDirectory,fileSystMounted, isMounted) = "Directories  " ++ printList(assocDir) ++ "\n" ++ "Files  "++ printList(assocFiles)

printList :: ([String])->String
printList ([]) = ""
printList (tmp:list) = tmp ++ "  " ++ printList(list)

printSpecificDirData :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],(String,String,[String],[String],String,String,String,String,String,Bool))->String
printSpecificDirData (directoriesList,filesList,(dirName,dirPath,assocDir,assocFiles,date,time,ownership,cdDirectory,fileSystMounted, isMounted)) = printAssocDir(assocDir,directoriesList) ++ printAssocFile(assocFiles,filesList)

printAssocDir :: ([String],[(String,String,[String],[String],String,String,String,String,String,Bool)])->String
printAssocDir ([],directoriesList) = ""
printAssocDir (tmpDir:dirList,directoriesList) = if findDir(directoriesList,tmpDir)
													then "d " ++ printDirInformation(getDir(directoriesList,tmpDir)) ++ printAssocDir (dirList,directoriesList)
													else printAssocDir (dirList,directoriesList)

printAssocFile :: ([String],[(String,String,String,String,String,String)])->String
printAssocFile ([],directoriesList) = ""
printAssocFile (tmpDir:dirList,directoriesList) = if findFile(directoriesList,tmpDir)
													then "- " ++ printFileInformation(getFile(directoriesList,tmpDir)) ++ printAssocFile (dirList,directoriesList)
													else printAssocFile (dirList,directoriesList)


printDirInformation :: (String,String,[String],[String],String,String,String,String,String,Bool)->String
printDirInformation (dirName,dirPath,assocDir,assocFiles,date,time,ownership,cdDirectory,fileSystMounted, isMounted) = ownership ++ "  " ++ date ++ "  " ++ time ++ "  " ++dirName ++ "\n"

printFileInformation :: (String,String,String,String,String,String)->String
printFileInformation (name,path,pdata,date,time,ownership) = ownership ++ "  " ++ date ++ "  " ++ time ++ "  " ++name ++ "\n"

printSpecificFileData :: (String,String,String,String,String,String)->String
printSpecificFileData (name,path,pdata,date,time,ownership) = ownership ++ "  " ++ date ++ "  " ++ time ++ "  " ++name ++ "\n"

printDirData :: (String,String,[String],[String],String,String,String,String,String,Bool)->String
printDirData (dirName,dirPath,assocDir,assocFiles,date,time,ownership,cdDirectory,fileSystMounted, isMounted) = ownership ++ "  " ++ date ++ "  " ++ time ++ "  " ++dirName ++ "\n"
------------------------------------------------------------------------------------------------------------------------------------

----------------------------------------------CHOWN FUNCTIONS-----------------------------------------------------------------------
chown :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
chown (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do chown, expected: chown -R user:group path or chown user:group path")
chown (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "-R"  = chownR (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| otherwise =  chownAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,x)
	

chownAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[String],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
chownAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],userGroup) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do chown, expected: chown user:group path")
chownAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,userGroup) 
	| verifyUserAndGroupChown(userGroup) = if (finduser(pUserList, head(splitOn ":" userGroup))) && (findgroup(pGroupList, last(splitOn ":" userGroup)))
									then if (length xs) == 0
										then chownAux2 (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x,userGroup)
										else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do chown, expected: path")
									else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do chown, user or group doesn't exists")										
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do chown, user and group format chould be user:group... " ++ x)



chownAux2 :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
chownAux2 (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,chownpath,userGroup)
	| (getDirCD(directoriesList) == "/") = if (findDirByPath(directoriesList,chownpath))								
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirOwnershipByPath(directoriesList,chownpath,userGroup),filesList),"")
												else if (findFileByPath(filesList,chownpath))								
													then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,updateFileOwnershipByPath (filesList,chownpath,userGroup)),"")
													else if (findFile(filesList,chownpath))
														then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,updateFileOwnershipByName(filesList,chownpath,userGroup)),"")
														else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ chownpath ++ " doesn't exists")
	| (getDirCD(directoriesList) /= "/") = if (findDirByPath(directoriesList,getDirCD(directoriesList) ++ chownpath))
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirOwnershipByPath(directoriesList, getDirCD(directoriesList) ++ chownpath,userGroup),filesList),"")
												else if findDir(directoriesList,chownpath)
													then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirOwnershipByName(directoriesList,getDirCD(directoriesList) ++ chownpath,userGroup),filesList),"")
													else if (findFileByPath(filesList,getDirCD(directoriesList) ++ chownpath))
														then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,updateFileOwnershipByPath (filesList,chownpath,userGroup)),"")
														else if (findFile(filesList,chownpath))
															then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,updateFileOwnershipByName(filesList,chownpath,userGroup)),"")
															else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ chownpath ++ " doesn't exists")


updateDirOwnershipByName :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String,String) -> [(String,String,[String],[String],String,String,String,String,String,Bool)]
updateDirOwnershipByName ([], dir,owner) = []
updateDirOwnershipByName (tmp:dirList,dirName,ownership)
	| dirName == sel1 tmp = (dirName,sel2 tmp,sel3 tmp, sel4 tmp,sel5 tmp , sel6 tmp,ownership,sel8 tmp, sel9 tmp, sel10 tmp) :	updateDirOwnershipByName (dirList,dirName,ownership)
	| otherwise = tmp : updateDirOwnershipByName (dirList,dirName,ownership)	

updateDirOwnershipByPath :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],String,String) -> [(String,String,[String],[String],String,String,String,String,String,Bool)]
updateDirOwnershipByPath ([], dir,owner) = []
updateDirOwnershipByPath (tmp:dirList,dirPath,ownership)
	| dirPath == sel2 tmp = (sel1 tmp,dirPath,sel3 tmp, sel4 tmp, sel5 tmp, sel6 tmp,ownership,sel8 tmp, sel9 tmp, sel10 tmp) :	updateDirOwnershipByPath (dirList,dirPath,ownership)
	| otherwise = tmp : updateDirOwnershipByPath (dirList,dirPath,ownership)	


updateFileOwnershipByName :: ([(String,String,String,String,String,String)],String,String) -> [(String,String,String,String,String,String)]
updateFileOwnershipByName ([], dir,owner) = []
updateFileOwnershipByName (tmp:dirList,fileName,ownership)
	| fileName == sel1 tmp = (fileName,sel2 tmp,sel3 tmp, sel4 tmp, sel5 tmp, ownership) : updateFileOwnershipByName (dirList,fileName,ownership)
	| otherwise = tmp : updateFileOwnershipByName (dirList,fileName,ownership)

updateFileOwnershipByPath :: ([(String,String,String,String,String,String)],String,String) -> [(String,String,String,String,String,String)]
updateFileOwnershipByPath ([], dir,owner) = []
updateFileOwnershipByPath (tmp:dirList,fileName,ownership)
	| fileName == sel2 tmp = (sel1 tmp,fileName,sel3 tmp, sel4 tmp, sel5 tmp, ownership) : updateFileOwnershipByPath (dirList,fileName,ownership)
	| otherwise = tmp : updateFileOwnershipByPath (dirList,fileName,ownership)


chownR :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
chownR (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do chown, expected: chown -R user:group path or chown user:group path")
chownR (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	|verifyUserAndGroupChown(x) = if (finduser(pUserList, head(splitOn ":" x))) && (findgroup(pGroupList, last(splitOn ":" x)))
									then if (length xs) == 1
										then chownRAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,head xs,x)
										else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do chown, expected: path")
									else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do chown, user or group doesn't exists")										
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"Couldn't do chown, user and group format chould be user:group ")

chownRAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
chownRAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,chownpath,userGroup)
	| (getDirCD(directoriesList) == "/") = if (findDirByPath(directoriesList,chownpath))								
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList, updateDirOwnershipByPath(updateAssoDirOwnership(directoriesList,sel3 (getDirByPath(directoriesList,chownpath)),userGroup),chownpath,userGroup),updateDirFilesOwnership(filesList, sel4 (getDirByPath(directoriesList,chownpath)),userGroup)),"")
												else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ chownpath ++ " doesn't exists")
	| (getDirCD(directoriesList) /= "/") = if (findDirByPath(directoriesList,getDirCD(directoriesList) ++ chownpath))
												then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirOwnershipByPath(updateAssoDirOwnership(directoriesList,sel3 (getDirByPath(directoriesList,getDirCD(directoriesList) ++chownpath)),userGroup), getDirCD(directoriesList) ++ chownpath,userGroup),updateDirFilesOwnership(filesList, sel4 (getDirByPath(directoriesList,getDirCD(directoriesList) ++chownpath)),userGroup)),"")
												else if findDir(directoriesList,chownpath)
													then (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,updateDirOwnershipByName(updateAssoDirOwnership(directoriesList,sel3 (getDir(directoriesList,chownpath)),userGroup),chownpath,userGroup),updateDirFilesOwnership(filesList, sel4 (getDir(directoriesList,chownpath)),userGroup)),"")
													else (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList),"The path " ++ chownpath ++ " doesn't exists")	




updateDirFilesOwnership:: ([(String,String,String,String,String,String)],[String],String) -> [(String,String,String,String,String,String)]
updateDirFilesOwnership (filesList,[],ownership) = filesList
updateDirFilesOwnership (filesList,tmp: listOfDirFiles,ownership) = updateDirFilesOwnership(updateFileOwnershipByName(filesList,tmp,ownership), listOfDirFiles,ownership)

updateAssoDirOwnership :: ([(String,String,[String],[String],String,String,String,String,String,Bool)], [String], String ) -> [(String,String,[String],[String],String,String,String,String,String,Bool)]
updateAssoDirOwnership (dirList,[],ownership) = dirList
updateAssoDirOwnership (dirList,tmp: listOfDirAssoc,ownership) = updateAssoDirOwnership (updateDirOwnershipByName(dirList,tmp,ownership),listOfDirAssoc,ownership)
--------------------------------------------------------------------------------chown ends -----------------------------------------------------------------------

