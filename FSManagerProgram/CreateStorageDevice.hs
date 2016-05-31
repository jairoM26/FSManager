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
createdev :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y,date,time)= (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the storage device, expected: -s size")
createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y,date,time)
     | x == "" = createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y,date,time)
     | x == "-s" && (length xs) >= 1 = createdevAux (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,xs,y,date,time)
     | otherwise = (pGroupList,pUserList, (storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't  find: " ++ x)
   

{-
-}
createdevAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
createdevAux (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y,date,time) =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the storage device, expected: Storage device size")
createdevAux (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y,date,time)
     | x == "" = createdevAux (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList ,xs, y,date,time)         
     | verifyNumber (head (splitOn "M" x)) && (length xs) == 1 = createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,read (head (splitOn "M" x)) :: Int,"M", xs,y,date,time)
	| verifyNumber (head (splitOn "G" x)) && (length xs) == 1 = createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,read (head (splitOn "G" x)) :: Int,"G", xs,y,date,time)
     | otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the storage device, expected: sizeG or sizeM")

{-
-}
createdevAux2 :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],Int,String,[[Char]],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,size,pSizeType,[], y,date,time) =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't create the storage device, expected: Storage device size")
createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,size,pSizeType,x:xs,y,date,time)
	| x == "" = createdevAux2 (pGroupList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,size,pSizeType, xs, y,date,time)
	| findStorage (storageList,x) = (pGroupList, pUserList, (storageList,pVolumeGroupsList,pLVolume), (pFileSystList,directoriesList,filesList),"Couldn't create the storage device: Storage device already existis")
	| verifyPath x  = (pGroupList, pUserList, (storageList ++ [(x,size,pSizeType,[False,False])],pVolumeGroupsList,pLVolume),(pFileSystList,(updateDirectories(directoriesList,[],"dev",drop 5 x,date,time)) ++ [createDirectory (drop 5 x,x,date,time)],filesList),"")
	| otherwise = (pGroupList, pUserList, (storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't create the storage device expected: /dev/StorageDeviceName")
	

{---(pGroupList, pUserList, (storageList ++ [(x,size,pSizeType,[False,False])],pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "")
-}
findStorage (storageList, x)
     | null storageList = False
     | (x `elem` (generatePathStorageDev storageList)) =  True
     | otherwise = False   