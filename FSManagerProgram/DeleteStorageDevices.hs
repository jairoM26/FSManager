{-# OPTIONS_GHC -fno-warn-tabs #-}
module DeleteStorageDevices
(rmdev
)
where 
import GetData
import CreateStorageDevice

{-
-}
rmdev :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
rmdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't delete the storage device, expected: Storage device name")
rmdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = rmdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	| findStorage (storageList,x) = deleteStorage (pGroupList, pUserList, storageList, [],pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x)
	| otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't  find: " ++ x)

{-
-}
deleteStorage :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
deleteStorage (pGroupList,pUserList,[],pStorageDevListToReturn,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,storageTodelete) = (pGroupList,pUserList,(pStorageDevListToReturn,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"")
deleteStorage (pGroupList,pUserList,storageTmp:pStorageDevList,pStorageDevListToReturn,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,storageTodelete) 
	|storageTodelete == tupleGetFirst storageTmp = if (head (tupleGetFourth storageTmp))
										then (pGroupList,pUserList,(pStorageDevList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't delete the storage device, the storage belongs to a volume group")
										else if (last (tupleGetFourth storageTmp))
												then (pGroupList,pUserList,(pStorageDevList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't delete the storage device, the storage have a file system mounted on it")
												else deleteStorage (pGroupList,pUserList,pStorageDevList,pStorageDevListToReturn,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,storageTodelete)
	|otherwise = deleteStorage (pGroupList,pUserList,pStorageDevList,pStorageDevListToReturn ++ [storageTmp],pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,storageTodelete)	
