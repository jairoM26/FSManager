{-# OPTIONS_GHC -fno-warn-tabs #-}
module ShowStorageDevices
(fdisk
)
where
import GetData

{-
-}
fdisk (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't show the storage devices, expected: fdisk -l")
fdisk (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList, directoriesList,filesList, x:xs, y)
	| x == "" = fdisk (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, xs, y)
	| x == "-l" = showStorageList (pGroupList,pUserList,storageList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, "")
	| otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't  find: " ++ x)


showStorageList (pGroupList,pUserList,storageList, [],pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, pMessageToShow) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),pMessageToShow)
showStorageList (pGroupList,pUserList,storageList, x:tmpList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessageToShow) 
	| head (tupleGetFourth x) = showStorageList (pGroupList, pUserList, storageList, tmpList, pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,
							pMessageToShow ++ "Disk   " ++ tupleGetFirst x ++ addspaces(20- length(tupleGetFirst x),"") ++ show(tupleGetSecond x) ++ " " ++ tupleGetThird x ++ "   Managed by:   LVM" ++ "\n")
	|otherwise = showStorageList (pGroupList, pUserList, storageList, tmpList, pVolumeGroupsList,pLVolume, pFileSystList,directoriesList,filesList,
							pMessageToShow ++ "Disk   " ++ tupleGetFirst x ++ addspaces(20- length(tupleGetFirst x),"") ++ show(tupleGetSecond x) ++ " " ++ tupleGetThird x ++ "\n")