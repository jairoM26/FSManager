{-# OPTIONS_GHC -fno-warn-tabs #-}
module CreateStorageDevice 
(createdev
,findStorage
)
where 
import GetData
import Data.List.Split-- library split

createdev:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,[],y)= (pGroupList,pUserList,(storageList,pVolumeGroupsList),"Couldn't create the storage device, expected: -s size")
createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,x:xs,y)
     | x == "" = createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,x:xs,y)
     | x == "-s" && (length xs) >= 1 = createdevAux (pGroupList, pUserList, storageList,pVolumeGroupsList,xs,y)
     | otherwise = (pGroupList,pUserList, (storageList,pVolumeGroupsList), "Couldn't  find: " ++ x)
   

createdevAux:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
createdevAux (pGroupList,pUserList,storageList,pVolumeGroupsList,[],y) =  (pGroupList,pUserList,(storageList,pVolumeGroupsList),"Couldn't create the storage device, expected: Storage device size")
createdevAux (pGroupList,pUserList,storageList,pVolumeGroupsList,x:xs,y)
     | x == "" = createdevAux (pGroupList, pUserList, storageList,pVolumeGroupsList, xs, y)         
     | verifyNumber (head (splitOn "M" x)) && (length xs) == 1 = createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,read (head (splitOn "M" x)) :: Int,"M", xs,y)
	 | verifyNumber (head (splitOn "G" x)) && (length xs) == 1 = createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,read (head (splitOn "G" x)) :: Int,"G", xs,y)
     | otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList),"Couldn't create the storage device, expected: sizeG or sizeM")

createdevAux2 :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],Int,String,[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,size,pSizeType,[], y) =  (pGroupList,pUserList,(storageList,pVolumeGroupsList),"Couldn't create the storage device, expected: Storage device size")
createdevAux2 (pGroupList,pUserList,storageList,pVolumeGroupsList,size,pSizeType,x:xs,y)
	| x == "" = createdevAux2 (pGroupList, pUserList, storageList,pVolumeGroupsList,size,pSizeType, xs, y)
	| findStorage (storageList,x) = (pGroupList, pUserList, (storageList,pVolumeGroupsList), "Couldn't create the storage device: Storage device already existis")
	| verifyPath x  = (pGroupList, pUserList, (storageList ++ [(x,size,pSizeType,[False,False])],pVolumeGroupsList) , "")
	| otherwise = (pGroupList, pUserList, (storageList,pVolumeGroupsList), "Couldn't create the storage device expected: /dev/StorageDeviceName")
	

verifyNumber :: String -> Bool
verifyNumber [] = True
verifyNumber (x:numberList)
	| ( x `elem` ['0'..'9'] )= verifyNumber numberList
	| otherwise = False

verifyPath :: String -> Bool
verifyPath pathList = if (take 5 pathList) == "/dev/"
						then True
						else False


findStorage:: ([(String,Int,String,[Bool])],String) -> Bool
findStorage (storageList, x)
     | null storageList = False
     | (x `elem` (generatePathStorageDev storageList)) =  True
     | otherwise = False   