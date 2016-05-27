{-# OPTIONS_GHC -fno-warn-tabs #-}
module VolumeGroups 
(pvcreate
,vgcreate
,vgreduce
,vgextend
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
	-@param pVolumeGroups = list o VG
	-@param xs = list with the instruction => x = element of the instruction
-}
pvcreate :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String) -> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
--If the list of instructions is emty it means the instruction was incomplete, in this case if its empty it means that the storage device name was forgotten
pvcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't update storage device and set it managed by LVM, expected: storage device name")
pvcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,x:xs,y) 
	| x == "" = pvcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,xs,y)
	--If the storage exists its mean that it can be managed by the LVM
	--(makeStorageManagedByLVM (pStorageList,[],x) ---> initialized the parameters, sends the pStorageList, a empty list and x = storage device name which will be managed by the LVM
	| findStorage (pStorageList,x) = (pGroupList,pUserList,(makeStorageManagedByLVM (pStorageList,[],x), pVolumeGroupList), "")
	--If the storage device doesn't exists
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't update storage device and set it managed by LVM: " ++ x ++ " doesn't exists")

{-
	-@brief Make the storage device managed by the LVM
	-@param pStorageList 
	-@param tmpStorageList = actual list
	-@param storageToManage = storagename who will be managed by the LVM
-}
makeStorageManagedByLVM :: ([(String,Int,String,[Bool])],[(String,Int,String,[Bool])],String) -> [(String,Int,String,[Bool])]
makeStorageManagedByLVM ([],tmpStorageList,x) = tmpStorageList
makeStorageManagedByLVM (tmpElement:pStorageList, tmpStorageList,storageToManage)
	| storageToManage ==  (sel1 (tmpElement) ) = makeStorageManagedByLVM (pStorageList, tmpStorageList ++ [(storageToManage, sel2 (tmpElement), sel3 (tmpElement), [True, last (sel4 (tmpElement))])],storageToManage)
	| otherwise = makeStorageManagedByLVM (pStorageList, tmpStorageList ++ [tmpElement],storageToManage)
----------------------------------- ------PVCREATE FUNCTIONS FINISH------------------------------------------------------------------------	

------------------------------------------VGCreate FUNCTIONS STARTS------------------------------------------------------------------------
vgcreate :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String) -> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
vgcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't create Volume Group, expected: Volume Group name")
vgcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,x:xs,y)
	| x == "" = vgcreate (pGroupList,pUserList,pStorageList,pVolumeGroupList,xs,y)
	| (verifyName x) = vgcreateAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,x,[],xs,y)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't  find: " ++ x)

vgcreateAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],String,[String],[[Char]],String) -> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
vgcreateAux (pGroupList,pUserList, pStorageList,pVolumeGroupList,vgName,physicalVolumeList,[], y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't create Volume Group, expected: physical volume")
vgcreateAux (pGroupList,pUserList, pStorageList,pVolumeGroupList,vgName,physicalVolumeList,x:xs, y)
	| x == "" = vgcreateAux (pGroupList,pUserList, pStorageList,pVolumeGroupList,vgName,physicalVolumeList,xs, y)
	| findStorage (pStorageList, x) && (length xs) >=1 = if head (sel4 (getDevice (pStorageList,x)))
															then (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't create Volume Group, expected: physical volume") 
															else vgcreateAux (pGroupList,pUserList, pStorageList,pVolumeGroupList,vgName,physicalVolumeList ++ [x],xs, y) 
	| null xs = vgcreateAux2 (pGroupList,pUserList, pStorageList,pVolumeGroupList,vgName,physicalVolumeList ++ [x])
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't find: " ++ x)

vgcreateAux2 :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],String,[String]) -> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
vgcreateAux2 (pGroupList,pUserList, pStorageList,pVolumeGroupList,vgName,physicalVolumeList) = (pGroupList,pUserList, addVG (pStorageList, pVolumeGroupList, vgName,physicalVolumeList,physicalVolumeList,0),"")

addVG :: ([((String,Int,String,[Bool]))],[(String,[String],[String],Int,Int)],String,[String],[String],Int)->([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)])
addVG (pStorageList,pVolumeGroupList,vgName, physicalVolumeList,[],totalSize) = (pStorageList,pVolumeGroupList ++ [(vgName,physicalVolumeList,[],totalSize,totalSize)])
addVG (pStorageList,pVolumeGroupList,vgName, physicalVolumeList,x:tmpList,totalSize) = addVG (pStorageList,pVolumeGroupList,vgName,physicalVolumeList,tmpList,totalSize + (getStorageSize (pStorageList,x)))
------------------------------------------VGCreate FUNCTIONS ENDS------------------------------------------------------------------------

------------------------------------------VgReduce FUNCTIONS STARTS------------------------------------------------------------------------
vgreduce :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String) -> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
vgreduce (pGroupList,pUserList,pStorageList,pVolumeGroupList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't reduce Volume Group, expected: Volume Group name")
vgreduce (pGroupList,pUserList,pStorageList,pVolumeGroupList,x:xs,y)
	| x == "" = vgreduce (pGroupList,pUserList,pStorageList,pVolumeGroupList,xs,y)
	| (findVG (pVolumeGroupList, x) && (length xs) == 1) = vgreduceAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,xs,x)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't find: " ++ x)

vgreduceAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String) -> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
vgreduceAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,[],vgName) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't reduce Volume Group, expected: physical name")
vgreduceAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,x:xs,vgName)
	| x == "" = vgreduceAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,xs,vgName)
	| findStorage (pStorageList,x) = (pGroupList, pUserList,(reduceVG (pStorageList,pVolumeGroupList,[],x,vgName)),"")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't reduce Volume Group, " ++ x ++ " is not a physical volume of " ++ vgName)

reduceVG :: ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,[String],[String],Int,Int)],String,String)->([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)])
reduceVG (pStorageList,[],tmpVolumeGroupList,toDelete,vgName) = (pStorageList,tmpVolumeGroupList)
reduceVG (pStorageList,tmp:pVolumeGroupList,tmpVolumeGroupList,toDelete,vgName)
	| vgName == (sel1 tmp) = reduceVG (pStorageList, pVolumeGroupList, tmpVolumeGroupList ++ [(vgName, deleteStorageFromVG ((sel2 tmp),[],toDelete),sel3 tmp, (sel4 tmp) -getStorageSize (pStorageList, toDelete),(sel5 tmp) -getStorageSize (pStorageList, toDelete))],toDelete,vgName)
	| otherwise = reduceVG (pStorageList,pVolumeGroupList,tmpVolumeGroupList ++ [tmp],toDelete,vgName)	


deleteStorageFromVG :: ([String],[String],String)->[String]
deleteStorageFromVG ([], tmpList, toDelete) = tmpList
deleteStorageFromVG (tmp:physicalVolumeList,tmpList,toDelete)
	| toDelete == tmp = deleteStorageFromVG(physicalVolumeList,tmpList,toDelete)
	| otherwise = deleteStorageFromVG (physicalVolumeList,tmpList ++ [tmp],toDelete)

------------------------------------------VGExtend FUNCTIONS STARTS------------------------------------------------------------------------
vgextend :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String) -> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
vgextend (pGroupList,pUserList,pStorageList,pVolumeGroupList,[],y) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't extend Volume Group, expected: Volume Group name")
vgextend (pGroupList,pUserList,pStorageList,pVolumeGroupList,x:xs,y)
	| x == "" = vgextend (pGroupList,pUserList,pStorageList,pVolumeGroupList,xs,y)
	| (findVG (pVolumeGroupList, x) && (length xs) == 1) = vgextendAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,xs,x)
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't find: " ++ x)

vgextendAux ::([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String) -> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
vgextendAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,[],vgName) = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't reduce Volume Group, expected: Volume Group name")
vgextendAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,x:xs,vgName)
	| x == "" = vgextendAux (pGroupList,pUserList,pStorageList,pVolumeGroupList,xs,vgName)
	--x = storage to add
	| findStorage (pStorageList,x)= (pGroupList, pUserList,(extendVG (pStorageList,pVolumeGroupList,[],x,vgName)),"")
	| otherwise = (pGroupList,pUserList,(pStorageList,pVolumeGroupList), "Couldn't reduce Volume Group, " ++ x ++ " is not a physical volume of " ++ vgName)	

extendVG :: ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,[String],[String],Int,Int)],String,String)->([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)])
extendVG (pStorageList,[],tmpVolumeGroupList,storageToAdd,vgName) = (pStorageList,tmpVolumeGroupList)
extendVG (pStorageList,tmpElement:pVolumeGroupList,tmpVolumeGroupList,storageToAdd,vgName)
	| vgName == (sel1 tmpElement) = extendVG (pStorageList, pVolumeGroupList, tmpVolumeGroupList ++ [(vgName, (sel2 tmpElement) ++ [storageToAdd], sel3 tmpElement, (sel4 tmpElement) + getStorageSize(pStorageList, storageToAdd),(sel5 tmpElement) + getStorageSize(pStorageList, storageToAdd))], storageToAdd,vgName)
	| otherwise = extendVG (pStorageList,pVolumeGroupList,tmpVolumeGroupList ++ [tmpElement],storageToAdd,vgName)
------------------------------------------VGExtend FUNCTIONS STARTS------------------------------------------------------------------------

findVG :: ([(String,[String],[String],Int,Int)],String)->Bool
findVG (pVolumeGroupList,pVG)
	| null pVolumeGroupList = False
    | (pVG `elem` (generateVGNames pVolumeGroupList)) =  True
    | otherwise = False 

