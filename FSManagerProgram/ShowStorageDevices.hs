{-# OPTIONS_GHC -fno-warn-tabs #-}
module ShowStorageDevices
(fdisk
)
where
import GetData

fdisk :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
fdisk (pGroupList,pUserList,storageList,pVolumeGroupsList,[],y) = (pGroupList,pUserList,(storageList,pVolumeGroupsList),"Couldn't show the storage devices, expected: fdisk -l")
fdisk (pGroupList,pUserList,storageList,pVolumeGroupsList, x:xs, y)
	| x == "" = fdisk (pGroupList, pUserList, storageList,pVolumeGroupsList, xs, y)
	| x == "-l" = showStorageList (pGroupList,pUserList,storageList,storageList,pVolumeGroupsList, "")
	| otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList), "Couldn't  find: " ++ x)


showStorageList :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
showStorageList (pGroupList,pUserList,storageList, [],pVolumeGroupsList, pMessageToShow) = (pGroupList,pUserList,(storageList,pVolumeGroupsList),pMessageToShow)
showStorageList (pGroupList,pUserList,storageList, x:tmpList,pVolumeGroupsList,pMessageToShow) 
	| head (tupleGetFourth x) = showStorageList (pGroupList, pUserList, storageList, tmpList, pVolumeGroupsList,
							pMessageToShow ++ "Disk   " ++ tupleGetFirst x ++ addspaces(20- length(tupleGetFirst x),"") ++ show(tupleGetSecond x) ++ " " ++ tupleGetThird x ++ "   Managed by:   LVM" ++ "\n")
	|otherwise = showStorageList (pGroupList, pUserList, storageList, tmpList, pVolumeGroupsList,
							pMessageToShow ++ "Disk   " ++ tupleGetFirst x ++ addspaces(20- length(tupleGetFirst x),"") ++ show(tupleGetSecond x) ++ " " ++ tupleGetThird x ++ "\n")