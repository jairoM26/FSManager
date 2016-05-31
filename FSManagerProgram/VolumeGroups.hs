{-# OPTIONS_GHC -fno-warn-tabs #-}
module VolumeGroups 
(pvcreate
,vgcreate
,vgreduce
,vgextend
,vlcreate
,findVG
,findVL
)
where 
import Data.List.Split-- library split
import Data.Tuple.Select
import CreateStorageDevice
import GetData

----------------------------------- PVCREATE FUNCTIONS STARTS------------------------------------------------------------------------
{-
	-@brief This function update a storage device and set it as managed by LVM, this managed allow to the storage device behave as 
			a physical volume
	-@param pGroupList = list of all the group created
	-@param pUserList = list of users
	-@param pStorageLIst = list of storages
	-@param pVolumeGroups = list of VG
	-@param pLVolume = list of logival volume
	-@param pFileSystList = list of file systems
	-@param xs = list with the instruction => x = element of the instruction
-}
--If the list of instructions is emty it means the instruction was incomplete, in this case if its empty it means that the storage device name was forgotten
pvcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't update storage device and set it managed by LVM, expected: storage device name")
pvcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y) 
	| x == "" = pvcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	--If the storage exists its mean that it can be managed by the LVM
	--(makeStorageManagedByLVM (pStorageList,[],x) ---> initialized the parameters, sends the pStorageList, a empty list and x = storage device name which will be managed by the LVM
	| findStorage (pStorageList,x) = (pGroupList,pUserList,(makeStorageManagedByLVM (pStorageList,[],x), pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "")
	--If the storage device doesn't exists
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't update storage device and set it managed by LVM: " ++ x ++ " doesn't exists")

{-
	-@brief Make the storage device managed by the LVM
	-@param pStorageList 
	-@param tmpStorageList = actual list
	-@param storageToManage = storagename who will be managed by the LVM
-}
makeStorageManagedByLVM ([],tmpStorageList,x) = tmpStorageList
makeStorageManagedByLVM (tmpElement:pStorageList, tmpStorageList,storageToManage)
	| storageToManage ==  (sel1 (tmpElement) ) = makeStorageManagedByLVM (pStorageList, tmpStorageList ++ [(storageToManage, sel2 (tmpElement), sel3 (tmpElement), [True, last (sel4 (tmpElement))])],storageToManage)
	| otherwise = makeStorageManagedByLVM (pStorageList, tmpStorageList ++ [tmpElement],storageToManage)
----------------------------------- ------PVCREATE FUNCTIONS FINISH------------------------------------------------------------------------	

------------------------------------------VGCreate FUNCTIONS STARTS------------------------------------------------------------------------
{-
	-@brief This function create a volume group
	-@param pGroupList = list of all the group created
	-@param pUserList = list of users
	-@param pStorageLIst = list of storages
	-@param pVolumeGroups = list of VG
	-@param pLVolume = list of logival volume
	-@param pFileSystList = list of file systems
	-@param directoriesList = list of directories
	-@param fileList = list of files
	-@param xs = list with the instruction => x = element of the instruction
-}
vgcreate :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
vgcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't create Volume Group, expected: Volume Group name")
vgcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = vgcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	--Verify the vg name, and then call vgcreateAux with [] wich is the list of all the storage devices managed by LVM
	--x = vgName
	| (verifyName x) = vgcreateAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x,[],xs,y)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't  find: " ++ x)


{-
	-@brief This function create a volume group
	-@param pGroupList = list of all the group created
	-@param pUserList = list of users
	-@param pStorageLIst = list of storages
	-@param pVolumeGroups = list of VG
	-@param pLVolume = list of logival volume
	-@param pFileSystList = list of file systems
	-@param directoriesList = list of directories
	-@param fileList = list of files
	-@param physicalVolumeList final list of all the storage devices managed by LVM
	-@param xs = list with the instruction => x = element of the instruction
-}
vgcreateAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],String,[String],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
vgcreateAux (pGroupList,pUserList, pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,vgName,physicalVolumeList,[], y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't create Volume Group, expected: physical volume")
vgcreateAux (pGroupList,pUserList, pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,vgName,physicalVolumeList,x:xs, y)
	| x == "" = vgcreateAux (pGroupList,pUserList, pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,vgName,physicalVolumeList,xs, y)
	--if the length of the storage list it's emty it means there is no storage devices created, so there is no storage device for created the vg
	| (length pStorageList) == 0 = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't create Volume Group, physical volume doesn't exists" )
	--if the storage is found and there command list have more storage devices to add
	| findStorage (pStorageList, x) && (length xs) >=1 = if head (sel4 (getDevice (pStorageList,x))) --verify if the storage device is managed by LVM
															--add the storage the physicalvolumeList
															then vgcreateAux (pGroupList,pUserList, pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,vgName,physicalVolumeList ++ [x],xs, y) 
															else (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create Volume Group, expected: physical volume") 
	--if the command list its empty there are no more storage devices to add															
	| null xs = if head (sel4 (getDevice (pStorageList,x))) --verify if the storage is managed by LVM
					--add the storage the physicalvolumeList
					then vgcreateAux2 (pGroupList,pUserList, pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,vgName,physicalVolumeList ++ [x])
					else (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't create Volume Group, expected: physical volume") 
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't find: " ++ x)

{-
	-@brief This function add the volume group to the list of the volumeGroups
	-@param pGroupList = list of all the group created
	-@param pUserList = list of users
	-@param pStorageLIst = list of storages
	-@param pVolumeGroups = list of VG
	-@param pLVolume = list of logival volume
	-@param pFileSystList = list of file systems
	-@param directoriesList = list of directories
	-@param fileList = list of files
	-@param physicalVolumeList final list of all the storage devices managed by LVM
-}
vgcreateAux2 (pGroupList,pUserList, pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,vgName,physicalVolumeList) = (pGroupList,pUserList, addVG (pStorageList, pVolumeGroupList,pLVolume, vgName,physicalVolumeList,physicalVolumeList,0),(pFileSystList,directoriesList,filesList),"")

{-
-}
addVG (pStorageList,pVolumeGroupList,pLVolume,vgName, physicalVolumeList,[],totalSize) = (pStorageList,pVolumeGroupList ++ [(vgName,physicalVolumeList,[],totalSize,totalSize)],pLVolume)
addVG (pStorageList,pVolumeGroupList,pLVolume,vgName, physicalVolumeList,x:tmpList,totalSize) = addVG (pStorageList,pVolumeGroupList,pLVolume,vgName,physicalVolumeList,tmpList,totalSize + (getStorageSize (pStorageList,x)))
------------------------------------------VGCreate FUNCTIONS ENDS------------------------------------------------------------------------

------------------------------------------VgReduce FUNCTIONS STARTS------------------------------------------------------------------------
{-
	-@brief This function reduce a volume group
	-@param pGroupList = list of all the group created
	-@param pUserList = list of users
	-@param pStorageLIst = list of storages
	-@param pVolumeGroups = list of VG
	-@param pLVolume = list of logival volume
	-@param pFileSystList = list of file systems
	-@param directoriesList = list of directories
	-@param fileList = list of files
	-@param xs = list with the instruction => x = element of the instruction
-}
vgreduce :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
vgreduce (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't reduce Volume Group, expected: Volume Group name")
vgreduce (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = vgreduce (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	--search for the vg to reduce and verify if there is the physical volume it wants to delete
	-- x = vgName
	| (findVG (pVolumeGroupList, x) && (length xs) == 1) = vgreduceAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,x)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't find: " ++ x)

{-
-}
vgreduceAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],vgName) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't reduce Volume Group, expected: physical name")
vgreduceAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,vgName)
	| x == "" = vgreduceAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,vgName)
	--x should be the physical volume name to reduce
	| findStorage (pStorageList,x) = if verifyDevInVg (pVolumeGroupList,x) --verify if the storage is in the vg
										then if (getVGFreeSize (pVolumeGroupList, vgName) > getStorageSize (pStorageList,x))--if there is enough free space 
											then (pGroupList, pUserList,(reduceVG (pStorageList,pVolumeGroupList,[],pLVolume,x,vgName)),(pFileSystList,directoriesList,filesList),"")
											else (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't reduce Volume Group, there is not enough space")
										else (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't reduce Volume Group, " ++ x ++ " is not a physical volume of "++ vgName)									
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't reduce Volume Group, " ++ x ++ " is not a physical volume of " ++ vgName)

{-
	-@brief this function reduce the size of the vg
-}
reduceVG (pStorageList,[],tmpVolumeGroupList,pLVolume,toDelete,vgName) = (pStorageList,tmpVolumeGroupList,pLVolume)
reduceVG (pStorageList,tmp:pVolumeGroupList,tmpVolumeGroupList,pLVolume,toDelete,vgName)
	| vgName == (sel1 tmp) = reduceVG (pStorageList, pVolumeGroupList, tmpVolumeGroupList ++ [(vgName, deleteStorageFromVG ((sel2 tmp),[],toDelete),sel3 tmp, (sel4 tmp) -getStorageSize (pStorageList, toDelete),(sel5 tmp) -getStorageSize (pStorageList, toDelete))],pLVolume,toDelete,vgName)
	| otherwise = reduceVG (pStorageList,pVolumeGroupList,tmpVolumeGroupList ++ [tmp],pLVolume,toDelete,vgName)	


{-
	-@brief this function delete the physical volume from the vg
-}
deleteStorageFromVG ([], tmpList, toDelete) = tmpList
deleteStorageFromVG (tmp:physicalVolumeList,tmpList,toDelete)
	| toDelete == tmp = deleteStorageFromVG(physicalVolumeList,tmpList,toDelete)
	| otherwise = deleteStorageFromVG (physicalVolumeList,tmpList ++ [tmp],toDelete)

------------------------------------------VGExtend FUNCTIONS STARTS------------------------------------------------------------------------
{-
-}
vgextend (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't extend Volume Group, expected: Volume Group name")
vgextend (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = vgextend (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	| (findVG (pVolumeGroupList, x) && (length xs) == 1) = vgextendAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,x)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't find: " ++ x)
{-
-}
vgextendAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],vgName) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't reduce Volume Group, expected: Volume Group name")
vgextendAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,vgName)
	| x == "" = vgextendAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,vgName)
	--x = storage to add
	| findStorage (pStorageList,x)= (pGroupList, pUserList,(extendVG (pStorageList,pVolumeGroupList,[],pLVolume,x,vgName)),(pFileSystList,directoriesList,filesList),"")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't reduce Volume Group, " ++ x ++ " is not a physical volume of " ++ vgName)	

{-
-}
extendVG (pStorageList,[],tmpVolumeGroupList,pLVolume,storageToAdd,vgName) = (pStorageList,tmpVolumeGroupList,pLVolume)
extendVG (pStorageList,tmpElement:pVolumeGroupList,tmpVolumeGroupList,pLVolume,storageToAdd,vgName)
	| vgName == (sel1 tmpElement) = extendVG (pStorageList, pVolumeGroupList, tmpVolumeGroupList ++ [(vgName, (sel2 tmpElement) ++ [storageToAdd], sel3 tmpElement, (sel4 tmpElement) + getStorageSize(pStorageList, storageToAdd),(sel5 tmpElement) + getStorageSize(pStorageList, storageToAdd))],pLVolume, storageToAdd,vgName)
	| otherwise = extendVG (pStorageList,pVolumeGroupList,tmpVolumeGroupList ++ [tmpElement],pLVolume,storageToAdd,vgName)
------------------------------------------VGExtend FUNCTIONS ENDS------------------------------------------------------------------------

------------------------------------------VLCREATE FUNCTIONS STARTS------------------------------------------------------------------------
{-
-}
vlcreate :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String)],[(String,String,String,String,String,String)]),String)
vlcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't create Logic Volume, expected: lvcreate -L")
vlcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = vlcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	| x == "-L" = vlcreateAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	| otherwise = (pGroupList,pUserList, (pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't  find: " ++ x)

{-
-}
vlcreateAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't create Logic Volume, expected: logical volume size")
vlcreateAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = vlcreateAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	| verifyNumber (head (splitOn "M" x)) = vlcreateAux2 (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,read (head (splitOn "M" x)) :: Int,"M",xs)
	| verifyNumber (head (splitOn "G" x)) = vlcreateAux2 (pGroupList,pUserList,pStorageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,read (head (splitOn "G" x)) :: Int,"G",xs)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the Logical Volume, expected: sizeG or sizeM")	

{-
-}
vlcreateAux2 (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,[]) = (pGroupList,pUserList,(storageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the Logical Volume, expected: -n")
vlcreateAux2 (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,x:xs)
	| x == "" = vlcreateAux2 (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,xs)
	| x == "-n" = vlcreateAux3 (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,xs)
	| otherwise = (pGroupList,pUserList, (storageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't  find: " ++ x)


{-
-}
vlcreateAux3 (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,[]) = (pGroupList,pUserList,(storageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the Logical Volume, expected: logical volume name, dev/vgname/vlname")
vlcreateAux3 (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,x:xs)
	| x == "" = vlcreateAux3 (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,xs)
	| findVL (pLVolume,x) && (length xs) ==1 = (pGroupList,pUserList,(storageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the Logical Volume, expected: logical volume already exists")
	| otherwise = addLVolume (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,x,xs)

{-
-}
addLVolume (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,lvName,[]) = (pGroupList,pUserList,(storageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the Logical Volume, expected: volume group name")
addLVolume (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,lvName,x:xs)
	| x == "" = addLVolume (pGroupList,pUserList,storageList,pVolumeGroupList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,lvName,xs)
	| findVG (pVolumeGroupList, x) = addLVolumeAux (pGroupList,pUserList,storageList,pVolumeGroupList,[],pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,lvName,x)
	| otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the Logical Volume, expected: volume group doesn't exists")

{-
-}
addLVolumeAux (pGroupList,pUserList,storageList,[],tmpList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,lvName,vgName) = (pGroupList,pUserList,(storageList,tmpList,pLVolume ++ [(lvName,size,False)]),(pFileSystList,directoriesList,filesList),"")
addLVolumeAux (pGroupList,pUserList,storageList,tmp:pVolumeGroupList,tmpList,pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,lvName,vgName)
	| vgName == (sel1 tmp) = if (sel5 tmp) >= size
								then addLVolumeAux (pGroupList,pUserList,storageList,pVolumeGroupList,tmpList ++ [(sel1 tmp, sel2 tmp, (sel3 tmp) ++ [lvName],sel4 tmp, (sel5 tmp) - size)],pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,lvName,vgName)
								else (pGroupList,pUserList,(storageList,pVolumeGroupList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the Logical Volume, expected: there is not enough free space")							
	| otherwise = addLVolumeAux (pGroupList,pUserList,storageList,pVolumeGroupList,tmpList ++ [tmp],pLVolume,pFileSystList,directoriesList,filesList,size,psizeType,lvName,vgName)								

findVG :: ([(String,[String],[String],Int,Int)],String)->Bool
findVG (pVolumeGroupList,pVG)
	| null pVolumeGroupList = False
    | (pVG `elem` (generateVGNames pVolumeGroupList)) =  True
    | otherwise = False 

verifyDevInVg :: ([(String,[String],[String],Int,Int)],String)->Bool
verifyDevInVg ([],devName) = False
verifyDevInVg (tmp:pVolumeGroupList, devName)
	| (devName `elem` (sel2 tmp)) = True
	| otherwise = verifyDevInVg (pVolumeGroupList,devName)

findVL :: ([(String,Int,Bool)],String)->Bool
findVL (pLVolume,lv)
	| null pLVolume = False
	| (lv`elem` (generateLVNames pLVolume)) =  True
	| otherwise = False 