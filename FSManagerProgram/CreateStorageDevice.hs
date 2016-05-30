{-# OPTIONS_GHC -fno-warn-tabs #-}
module CreateStorageDevice 
(createdev
,findStorage
)
where 
import GetData
import Data.List.Split-- library split

---------------------------------------------------------------------
{-
-}
createdev :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y)= (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the storage device, expected: -s size")
createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
     | x == "" = createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
     | x == "-s" && (length xs) >= 1 = createdevAux (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
     | otherwise = (pGroupList,pUserList, (storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't  find: " ++ x)
   

{-
-}
createdevAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
createdevAux (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y) =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the storage device, expected: Storage device size")
createdevAux (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
     | x == "" = createdevAux (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList ,xs, y)         
     | verifyNumber (head (splitOn "M" x)) && (length xs) == 1 = createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,read (head (splitOn "M" x)) :: Int,"M", xs,y)
	| verifyNumber (head (splitOn "G" x)) && (length xs) == 1 = createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,read (head (splitOn "G" x)) :: Int,"G", xs,y)
     | otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the storage device, expected: sizeG or sizeM")

{-
-}
createdevAux2 :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],Int,String,[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,size,pSizeType,[], y) =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the storage device, expected: Storage device size")
createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,size,pSizeType,x:xs,y)
	| x == "" = createdevAux2 (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,size,pSizeType, xs, y)
	| findStorage (storageList,x) = (pGroupList, pUserList, (storageList,pVolumeGroupsList,pLVolume), (pFileSystList,directoriesList,filesList),"Couldn't create the storage device: Storage device already existis")
	| verifyPath x  = (pGroupList, pUserList, (storageList ++ [(x,size,pSizeType,[False,False])],pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "")
	| otherwise = (pGroupList, pUserList, (storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't create the storage device expected: /dev/StorageDeviceName")
	

{-
-}
findStorage (storageList, x)
     | null storageList = False
     | (x `elem` (generatePathStorageDev storageList)) =  True
     | otherwise = False   