{-# OPTIONS_GHC -fno-warn-tabs #-}
module DeleteVolume
(vgremove
,lvremove
)
where
import Data.Tuple.Select
import Data.List.Split-- library split
import GetData
import VolumeGroups

----------------------------------- VGREMOVE FUNCTIONS STARTS------------------------------------------------------------------------
{-
-}
vgremove :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
vgremove (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't remove volume groups, expected vg name")
vgremove (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = vgremove (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	| findVG (pVolumeGroupList,x) && (verifyLV (getVolsFromVG (pLVolume, sel3 (getVG (pVolumeGroupList,x)))))= removeVG (pGroupList,pUserList,pStorageList,pVolumeGroupList,[],pLVolume,pFileSystList,directoriesList,filesList,x)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't find: " ++ x )


{-
-}
removeVG :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
removeVG (pGroupList,pUserList,pStorageList,[],tmpList,pLVolume,pFileSystList,directoriesList,filesList,vgName) = (pGroupList,pUserList,(pStorageList,tmpList,pLVolume),(pFileSystList,directoriesList,filesList),"")
removeVG (pGroupList,pUserList,pStorageList,tmp:pVolumeGroupList,tmpList,pLVolume,pFileSystList,directoriesList,filesList,vgName)
	| vgName == (sel1 tmp) = removeVG (pGroupList,pUserList,(getDevsNotFromVG (pStorageList, sel2 tmp)) ++ (updateStorageDev(getDevsFromVG (pStorageList, sel2 tmp))),pVolumeGroupList,tmpList,pLVolume,pFileSystList,directoriesList,filesList,vgName)
	| otherwise = removeVG (pGroupList,pUserList,pStorageList,pVolumeGroupList,tmpList ++ [tmp],pLVolume,pFileSystList,directoriesList,filesList,vgName)
----------------------------------- VGREMOVE FUNCTION ENDS------------------------------------------------------------------------

----------------------------------- LVREMOVE FUNCTIONS STARTS------------------------------------------------------------------------
{-
-}
lvremove :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
lvremove (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't remove logical volume, expected dev/vgName/lvName")
lvremove (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = lvremove (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	| (length pLVolume) == 0 = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't remove logical volume, logical volume doesn't exists")
	| verifyPath (x) = lvremoveAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,head (tail (tail(splitOn  "/" x))),last (tail (tail(splitOn  "/" x))))
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't find: " ++ x )


{-
-}
lvremoveAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,vgName,lvName)
	| findVG (pVolumeGroupList,vgName) && findVL (pLVolume,lvName) =  if (sel3 (getLV (pLVolume,lvName)))
																		then (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't remove " ++ lvName ++ ", lv is mounted as a file system" )
																		else (pGroupList,pUserList, removeLV (pStorageList,pVolumeGroupList,pLVolume, vgName,lvName),(pFileSystList,directoriesList,filesList),"")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't find: /dev/" ++ vgName ++ "/" ++ lvName)
----------------------------------- LVREMOVE FUNCTION ENDS------------------------------------------------------------------------
{-
-}
removeLV (pStorageList,pVolumeGroupList,pLVolume,vgName,lvName) = (pStorageList,removeLVFromVG (pVolumeGroupList,[],pLVolume, vgName, lvName), removeLVFromLV (pLVolume,[],lvName))

{-
-}
removeLVFromVG ([], tmpVGList,pLVolume,vgName,lvName) = tmpVGList
removeLVFromVG (tmp:pVolumeGroupList,tmpVGList,pLVolume, vgName, lvName)
	| vgName == sel1 tmp = removeLVFromVG (pVolumeGroupList, tmpVGList ++ [(sel1 tmp, sel2 tmp, deleteLV (sel3 tmp, lvName),sel4 tmp, sel5 tmp + (sel2 (getLV (pLVolume,lvName))))],pLVolume,vgName,lvName)
	| otherwise = removeLVFromVG (pVolumeGroupList, tmpVGList ++ [tmp],pLVolume,vgName, lvName)

{-
-}
deleteLV ([],lvName) = []
deleteLV (tmp:lvNameList,lvName)
	| lvName == tmp = deleteLV(lvNameList,lvName)
	| otherwise = tmp : deleteLV(lvNameList,lvName)

{-
-}
removeLVFromLV ([],tmpLVList,lvName) = tmpLVList
removeLVFromLV (tmp:pLVolume, tmpLVList,lvName)
	| lvName == sel1 tmp = removeLVFromLV (pLVolume,tmpLVList,lvName)
	| otherwise = removeLVFromLV (pLVolume, tmpLVList ++ [tmp],lvName)

{-
-}
verifyLV :: [(String,Int,Bool)]->Bool
verifyLV [] = True
verifyLV (tmp:pLVolume)
	| not (sel3 tmp) = False
	| otherwise = verifyLV pLVolume

{-
-}
updateStorageDev :: [(String,Int,String,[Bool])]->[(String,Int,String,[Bool])]
updateStorageDev ([]) = []
updateStorageDev (tmp:pStorageList) = (sel1 tmp, sel2 tmp, sel3 tmp , [False, last (sel4 (tmp))]) : updateStorageDev (pStorageList)


{-
-}
getDevsFromVG (pStorageList,[]) = []
getDevsFromVG (tmpDev:pStorageList, tmpDevName:vgDevs)
	| tmpDevName == sel1 tmpDev = tmpDev : getDevsFromVG (pStorageList,vgDevs)
	| otherwise = getDevsFromVG (pStorageList,vgDevs)



{-
-}
getDevsNotFromVG (pStorageList,[]) = []
getDevsNotFromVG (tmpDev:pStorageList, tmpDevName:vgDevs)
	| tmpDevName /= sel1 tmpDev = tmpDev : getDevsNotFromVG (pStorageList,vgDevs)
	| otherwise = getDevsFromVG (pStorageList,vgDevs)

{-
-}
getVolsFromVG (pLVolume,[]) = pLVolume
getVolsFromVG (tmpLV:pLVolume,tmpLVName:vgLV)
	| tmpLVName == sel1 tmpLV = tmpLV : getVolsFromVG (pLVolume,vgLV)
	| otherwise = getVolsFromVG (pLVolume,vgLV)
