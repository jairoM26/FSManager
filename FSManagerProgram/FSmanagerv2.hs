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
import ShowVolumeGroups
import DeleteVolume
import LinkedList
import FileSystem
import GetData
import Data.Tuple.Select
import Data.Time

{-
     Group Info = (groupName, GroupID, UserAssocList)   
     UserInfo = (userName, UID, primaryGroup, SecondGroupList) 
     storageDevice = (size,SizeType,path, [isPhysicalVolm,isFileSystem])
     logical volumes = [(lvName,size,isFileSystem)]
     volume groups = (VGname, StorageDevList, LogicalVolumeList,TotalSize, FreeSize)
     fileSystem (path,extType,lv or dev,listofdirectories,size,mountPath)
     directoryData = (name,path,[dir],[files],date,time,ownership,cdDirectory,fileSystMounted)
     fileData = (name,path,data,date,time,ownership)
-}
{-
 --Main functions
 --Calls tha principal function with initial parameters
-}
--Infinite loop
main ::  IO()
main =  do 
  myTime <- getZonedTime
 --groups,users,(storagedevices, volumegroups, logicalvolumes),(filesystem,),messageToPrint)
  fsManager ([],[],([],[],[]),([],[("/","/",[],[],head (splitOn " " (formatTime defaultTimeLocale "%m:%d:%Y %T" myTime)),last (splitOn " " (formatTime defaultTimeLocale "%F %T" myTime)),"root:root","/","",False),("dev","/dev",[],[],head (splitOn " " (formatTime defaultTimeLocale "%m:%d:%Y %T" myTime)),last (splitOn " " (formatTime defaultTimeLocale "%F %T" myTime)),"root:root","/","",False)],[])," ")
     
{-
  --FSMANAGER FORMAT
     fsManager (groupList, userList, messageToprint)
  --This function take the instructions from the FSManagerConsole (interfaz) and starts calling function that will modify the list according
    to the input instruction
  --This functions calls itself recursively with the Total parameteres modified
-}
fsManager :: ([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)-> IO()
fsManager pGeneralList = do
    if (length (sel5 pGeneralList)) /= 0
          then putStrLn (sel5 pGeneralList) 
          else putStr ""
    putStr ( sel8 (head (sel2 (sel4 (pGeneralList)))) ++ ":~ ")
    inputInstruction <- getLine --Get the input instrction
    --putStrLn (show (tail(tail (splitOn  "/" inputInstruction))))
     {-
        --Send the input instruction to another function who will decodify and verify the input, and the apply the correct action
        --splitOn " " inputInstruction generate a list, make a split in every " " in the input
        --tupleGetFirst' pGeneralList = List of groups
        --tupleGetSecond' pGeneralList = List of users
     -}
    myTime <- getZonedTime
    fsManager (input (sel1 (pGeneralList), sel2 (pGeneralList), sel1 (sel3 (pGeneralList)), sel2 (sel3 (pGeneralList)),sel3 (sel3 (pGeneralList)), sel1 (sel4 (pGeneralList)),sel2 (sel4 (pGeneralList)), sel3 (sel4 (pGeneralList)),splitOn  " " inputInstruction,inputInstruction,head (splitOn " " (formatTime defaultTimeLocale "%m:%d:%Y %T" myTime)),last (splitOn " " (formatTime defaultTimeLocale "%m:%d:%Y %T" myTime))))
        
--Check what is the first command
--According to the first command calls function that will apply that command
{-   
-}
input :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String,String,String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
input (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,[],y,date,time)= (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSysList,directoriesList,filesList),"")
input (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,x:xs,y,date,time)
      {-| x=="groupadd"  = groupadd (pGroupList,pUserList,xs,y)
      | x=="useradd"= useradd (pGroupList,pUserList,xs,y)
      | x=="show"= showatributes (pGroupList,pUserList,xs,y)
      | x=="finger" = showUsersAux (pGroupList,pUserList,pUserList,"",xs)-}
      | x == "createdev" = createdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y,date,time)
      | x == "fdisk" = fdisk (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "rmdev" = rmdev (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "pvcreate" = pvcreate (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "vgcreate" = vgcreate (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "vgreduce" = vgreduce (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "vgextend" = vgextend (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "vgdisplay" || x == "vgs" = vgdisplay (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "lvcreate" = vlcreate (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y,date,time)
      | x == "vgremove" = vgremove (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x =="lvremove" = lvremove (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "mkfs" = mkfs (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "mkdir" = mkdir (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y,date,time)
      | x == "cd" = cd (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "touch" = touch (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y,date,time)
      | x == "echo" = echo (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y,date,time)
      | x == "rm" = rm (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSysList,directoriesList,filesList,xs,y)
      | x == "userList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSysList,directoriesList,filesList), show pUserList)
      | x == "groupList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList,pLVolume), (pFileSysList,directoriesList,filesList),show pGroupList)
      | x == "volumeList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSysList,directoriesList,filesList), show pVolumeGroupsList)
      | x == "devList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList,pLVolume), (pFileSysList,directoriesList,filesList),show storageList)
      | x == "lvList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList,pLVolume), (pFileSysList,directoriesList,filesList),show pLVolume)
      | x == "fsList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList,pLVolume), (pFileSysList,directoriesList,filesList),show pFileSysList) 
      | x == "dirList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList,pLVolume), (pFileSysList,directoriesList,filesList),show directoriesList) 
      | x == "filesList" = (pGroupList, pUserList,(storageList,pVolumeGroupsList,pLVolume), (pFileSysList,directoriesList,filesList),show filesList) 
      | otherwise = (pGroupList,pUserList, (storageList,pVolumeGroupsList,pLVolume),(pFileSysList,directoriesList,filesList), "Couldn't  find: " ++ x)
