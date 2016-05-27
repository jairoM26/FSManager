{-# OPTIONS_GHC -fno-warn-tabs #-}
module DeleteStorageDevices
(rmdev
)
where 
import GetData
import CreateStorageDevice

rmdev :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
rmdev (pGroupList,pUserList,storageList,pVolumeGroupsList,[],y) = (pGroupList,pUserList,(storageList,pVolumeGroupsList),"Couldn't delete the storage device, expected: Storage device name")
rmdev (pGroupList,pUserList,storageList,pVolumeGroupsList,x:xs,y)
	| x == "" = rmdev (pGroupList,pUserList,storageList,pVolumeGroupsList,xs,y)
	| findStorage (storageList,x) = deleteStorage (pGroupList, pUserList, storageList, [],pVolumeGroupsList,x)
	| otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList), "Couldn't  find: " ++ x)


deleteStorage :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
deleteStorage (pGroupList,pUserList,[],pStorageDevListToReturn,pVolumeGroupsList,storageTodelete) = (pGroupList,pUserList,(pStorageDevListToReturn,pVolumeGroupsList),"")
deleteStorage (pGroupList,pUserList,storageTmp:pStorageDevList,pStorageDevListToReturn,pVolumeGroupsList,storageTodelete) 
	|storageTodelete == tupleGetFirst storageTmp = if (head (tupleGetFourth storageTmp))
										then (pGroupList,pUserList,(pStorageDevList,pVolumeGroupsList),"Couldn't delete the storage device, the storage belongs to a volume group")
										else if (last (tupleGetFourth storageTmp))
												then (pGroupList,pUserList,(pStorageDevList,pVolumeGroupsList),"Couldn't delete the storage device, the storage have a file system mounted on it")
												else deleteStorage (pGroupList,pUserList,pStorageDevList,pStorageDevListToReturn,pVolumeGroupsList,storageTodelete)
	|otherwise = deleteStorage (pGroupList,pUserList,pStorageDevList,pStorageDevListToReturn ++ [storageTmp],pVolumeGroupsList,storageTodelete)	
