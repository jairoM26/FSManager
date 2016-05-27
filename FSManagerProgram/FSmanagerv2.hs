{-# OPTIONS_GHC -fno-warn-tabs #-}
-- @author: Jairo Méndez 
-- @author: Cristian Castillo
-- @date: 18 - 05 -2016
-- @File: FSManager.hs
-- @brief: A virtual file system manager

import Control.Monad  -- library that control the secuense of the program, like the command forever
import Data.List.Split-- library split

import CreateStorageDevice
import ShowStorageDevices
import DeleteStorageDevices
import VolumeGroups
import LinkedList
import GetData
import Data.Tuple.Select

{-
     Group Info = (groupName, GroupID, UserAssocList)   
     UserInfo = (userName, UID, primaryGroup, SecondGroupList) 
     storageDevice = (size,SizeType,path, [isPhysicalVolm,isFileSystem])
     volume groups = (VGname, StorageDevList, LogicalVolumeList,TotalSize, FreeSize)
-}
{-
 --Main functions
 --Calls tha principal function with initial parameters
-}
--Infinite loop
main ::  IO()
main =  do 
     fsManager ([],[],([],[])," ")
     
    
{-
  --FSMANAGER FORMAT
     fsManager (groupList, userList, messageToprint)
  --This function take the instructions from the FSManagerConsole (interfaz) and starts calling function that will modify the list according
    to the input instruction
  --This functions calls itself recursively with the Total parameteres modified
-}
fsManager :: ([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)-> IO()
fsManager pGeneralList = do
    if (length (sel4 pGeneralList)) /= 0
          then putStrLn (sel4 pGeneralList) 
          else putStr ""
    putStr ( "root$root: ")
    inputInstruction <- getLine --Get the input instrction
     {-
        --Send the input instruction to another function who will decodify and verify the input, and the apply the correct action
        --splitOn " " inputInstruction generate a list, make a split in every " " in the input
        --tupleGetFirst' pGeneralList = List of groups
        --tupleGetSecond' pGeneralList = List of users
     -}
    fsManager (input (sel1 (pGeneralList), sel2 (pGeneralList), sel1 (sel3 (pGeneralList)), sel2 (sel3 (pGeneralList)),splitOn  " " inputInstruction,inputInstruction))
        
--Check what is the first command
--According to the first command calls function that will apply that command
{-
   -- ([(String,Int,[String])],[[Char]],String) = pGroupList, CommandList, Command    
-}
input :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])], ([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)]),String)
input (pGroupList,pUserList,storageList,pVolumeGroupsList,[],y)= (pGroupList,pUserList,(storageList,pVolumeGroupsList),"")
input (pGroupList,pUserList,storageList,pVolumeGroupsList,x:xs,y)
      {-| x=="groupadd"  = groupadd (pGroupList,pUserList,xs,y)
      | x=="useradd"= useradd (pGroupList,pUserList,xs,y)
      | x=="show"= showatributes (pGroupList,pUserList,xs,y)
      | x=="finger" = showUsersAux (pGroupList,pUserList,pUserList,"",xs)-}
      | x == "createdev" = createdev (pGroupList,pUserList,storageList,pVolumeGroupsList, xs,y)
      | x == "fdisk" = fdisk (pGroupList,pUserList,storageList,pVolumeGroupsList,xs,y)
      | x == "rmdev" = rmdev (pGroupList,pUserList,storageList,pVolumeGroupsList,xs,y)
      | x == "pvcreate" = pvcreate (pGroupList,pUserList,storageList,pVolumeGroupsList,xs,y)
      | x == "vgcreate" = vgcreate (pGroupList,pUserList,storageList,pVolumeGroupsList,xs,y)
      | x == "vgreduce" = vgreduce (pGroupList,pUserList,storageList,pVolumeGroupsList,xs,y)
      | x == "vgextend" = vgextend (pGroupList,pUserList,storageList,pVolumeGroupsList,xs,y)
      | x == "userList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList), show pUserList)
      | x == "groupList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList), show pGroupList)
      | x == "volumeList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList), show pVolumeGroupsList)
      | x == "devList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList), show storageList)
      | otherwise = (pGroupList,pUserList, (storageList,pVolumeGroupsList), "Couldn't  find: " ++ x)
