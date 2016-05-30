{-# OPTIONS_GHC -fno-warn-tabs #-}
module FileSystem 
(mkfs
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
mkfs :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't make file system, expected: -t type path")
mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| x == "-t" = mkfs (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,xs,y)
	| (x == "ext2" || x == "ext3" || x == "ext4") && ((length xs) == 1) = mkfsAux (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,x, head xs)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't find: " ++ x)

{-
-}
mkfsAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
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
createFS ::([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],String,String,Char)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
createFS (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,pfsType,fpath,pathType) 
	| pathType == '1' = (pGroupList,pUserList,(updateStorageDevices (pStorageList,fpath),pVGList,pLVList),(pFileSystList ++ [(fpath,pfsType,pathType,[], getStorageSize(pStorageList,fpath))],directoriesList,filesList),"")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVGList, updateLV (pLVList,last (tail (tail(splitOn  "/" fpath))))),(pFileSystList ++ [(fpath,pfsType,pathType,[], getLVSize(pLVList,last (tail (tail(splitOn  "/" fpath)))))],directoriesList,filesList),"" )

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














