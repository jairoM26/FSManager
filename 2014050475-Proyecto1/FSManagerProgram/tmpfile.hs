createdev:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),String)
createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,[],y)= (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),"Couldn't create the storage device, expected: -s size")
createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,x:xs,y)
     | x == "" = createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,x:xs,y)
     | x == "-s" && (length xs) >= 1 = createdevAux (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume,xs,y)
     | otherwise = (pGroupList,pUserList, (storageList,pVolumeGroupsList,pLVolume), "Couldn't  find: " ++ x)
   

createdevAux:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),String)
createdevAux (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,[],y) =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),"Couldn't create the storage device, expected: Storage device size")
createdevAux (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,x:xs,y)
     | x == "" = createdevAux (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume, xs, y)         
     | verifyNumber (head (splitOn "M" x)) && (length xs) == 1 = createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,read (head (splitOn "M" x)) :: Int,"M", xs,y)
	 | verifyNumber (head (splitOn "G" x)) && (length xs) == 1 = createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,read (head (splitOn "G" x)) :: Int,"G", xs,y)
     | otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),"Couldn't create the storage device, expected: sizeG or sizeM")

createdevAux2 :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],Int,String,[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),String)
createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,size,pSizeType,[], y) =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),"Couldn't create the storage device, expected: Storage device size")
createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,size,pSizeType,x:xs,y)
	| x == "" = createdevAux2 (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume,size,pSizeType, xs, y)
	| findStorage (storageList,x) = (pGroupList, pUserList, (storageList,pVolumeGroupsList,pLVolume), "Couldn't create the storage device: Storage device already existis")
	| verifyPath x  = (pGroupList, pUserList, (storageList ++ [(x,size,pSizeType,[False,False])],pVolumeGroupsList,pLVolume) , "")
	| otherwise = (pGroupList, pUserList, (storageList,pVolumeGroupsList,pLVolume), "Couldn't create the storage device expected: /dev/StorageDeviceName")
	


findStorage:: ([(String,Int,String,[Bool])],String) -> Bool
findStorage (storageList, x)
     | null storageList = False
     | (x `elem` (generatePathStorageDev storageList)) =  True
     | otherwise = False   