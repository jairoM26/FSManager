{-# OPTIONS_GHC -fno-warn-tabs #-}
module ShowAll(
showall
)
where 
import GetData
import ShowVolumeGroups
import Data.Tuple.Select

showall :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
showall (pGroupList,pUserList,pStorageList,pVGList,pLVolumeList, pFileSystList, directoriesList,filesList,xs,y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVolumeList),(pFileSystList,directoriesList,filesList),getAll((pGroupList,pUserList,pStorageList,pVGList,pLVolumeList, pFileSystList, directoriesList,filesList)))


getAll :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)])->String
getAll (pGroupList,pUserList,pStorageList,pVGList,pLVolumeList, pFileSystList, directoriesList,filesList) = generateGroupsShow(pGroupList) ++ generateUserShow(pUserList) ++ generateDevicesShow(pStorageList) ++ generateVolumesShow(pVGList,pStorageList,pLVolumeList) ++ generateFileSystemShow(pFileSystList) ++ generateDirectoriesAndFilesShow(directoriesList,filesList)

------------------Generate string for groups--------------------------
generateGroupsShow :: ([(String,Int,[String])])-> String
generateGroupsShow ([]) = ""
generateGroupsShow (pGroupList) = "-----------------------------Groups Starts-----------------------------------------------------------------" ++ "\n" ++"GroupName                          GID           AssociatedUsers " ++ "\n" ++ generateGroupsShowAux(pGroupList) ++ "\n" ++ "-----------------------------Groups Ends-----------------------------------------------------------------" ++"\n"

generateGroupsShowAux :: ([(String,Int,[String])])-> String
generateGroupsShowAux ([]) = ""
generateGroupsShowAux (tmpGroup : pGroupList) = sel1 tmpGroup ++ addspaces( 36- length(sel1 tmpGroup),"") ++ show(sel2 tmpGroup) ++ addspaces( 10,"") ++ generateAssocUserGroupString (sel3 tmpGroup) ++  "\n" ++ generateGroupsShowAux(pGroupList)

------------------Generate string for users--------------------------
generateUserShow :: ([(String,Int,String,[String])])-> String
generateUserShow ([]) = ""
generateUserShow (pUserList) = "-----------------------------Users Starts-----------------------------------------------------------------" ++ "\n" ++ "UserName                            UID           primaryGroup                        SecondaryGroup                      HomeDirectory" ++ "\n" ++ generateUserShowAux(pUserList) ++ "-----------------------------Users Starts-----------------------------------------------------------------" ++ "\n"

generateUserShowAux :: ([(String,Int,String,[String])])-> String
generateUserShowAux ([]) = ""
generateUserShowAux (tmpUser : pUserList) = sel1 tmpUser ++ addspaces(36- length(sel1 tmpUser),"") ++ show(sel2 tmpUser)   ++ addspaces( 10,"") ++ (sel3 tmpUser) ++ addspaces( 36- length(sel3 tmpUser),"") ++ generateSecondaryGroupNames (sel4 tmpUser)++ addspaces( 36- length(generateSecondaryGroupNames (sel4 tmpUser)),"") ++ "/home/" ++ sel1 tmpUser ++ "\n" ++ generateUserShowAux(pUserList)

------------------Generate string for storage devices--------------------------
generateDevicesShow :: ([(String,Int,String,[Bool])])-> String
generateDevicesShow ([]) = ""
generateDevicesShow (pStorageList) = "-----------------------------Storage Devices Starts-----------------------------------------------------------------" ++ "\n" ++ generateDevicesShowAux(pStorageList) ++ "-----------------------------Storage Devices Ends-----------------------------------------------------------------" 

generateDevicesShowAux :: ([(String,Int,String,[Bool])])-> String
generateDevicesShowAux ([]) = ""
generateDevicesShowAux (tmpDev : pStorageList)
	|head (sel4 (tmpDev)) = "Disk   " ++ sel1 tmpDev ++ addspaces(20- length(sel1 tmpDev),"") ++ show(sel2 tmpDev) ++ " " ++ sel3 tmpDev ++ "   Managed by:   LVM" ++ "\n" ++ generateDevicesShowAux(pStorageList)
	| otherwise = "Disk   " ++ sel1 tmpDev ++ addspaces(20- length(sel1 tmpDev),"") ++ show(sel2 tmpDev) ++ " " ++ sel3 tmpDev ++ "\n" ++ generateDevicesShowAux(pStorageList)

------------------Generate string for group volume--------------------------
generateVolumesShow::([(String,[String],[String],Int,Int)],[(String,Int,String,[Bool])],[(String,Int,Bool)])->String
generateVolumesShow ([],pStorageList,pLVolumeList) = ""
generateVolumesShow (pVGList,pStorageList,pLVolumeList) = "-----------------------------Volume Groups Starts-----------------------------------------------------------------" ++ "\n" ++ generateVolumesShowAux (pVGList,pStorageList,pLVolumeList) ++ "-----------------------------Volume Groups Ends-----------------------------------------------------------------" ++ "\n" 


generateVolumesShowAux ::([(String,[String],[String],Int,Int)],[(String,Int,String,[Bool])],[(String,Int,Bool)])->String
generateVolumesShowAux ([],pStorageList,pLVolumeList) = ""
generateVolumesShowAux (tmpVG : pVGList,pStorageList,pLVolumeList) = "-------------- volume Group ------------" ++ "\n" ++ getSimpleMessage(tmpVG) ++ "\n" ++ "-------------- Logical Volum ------------" ++ "\n" ++ (getLVMessage (pLVolumeList,sel3 tmpVG)) ++ "\n" ++ "--- Physical Volume---" ++ "\n" ++(getPVMessage (pStorageList,sel2 tmpVG)) ++ "\n" ++ generateVolumesShowAux(pVGList,pStorageList,pLVolumeList)

------------------Generate string for file system--------------------------
generateFileSystemShow :: ([(String,String,Char,[String],Int,String)]) -> String
generateFileSystemShow ([]) = ""
generateFileSystemShow (pFileSystList) = "-----------------------------Files Sistems Starts-----------------------------------------------------------------" ++ "\n" ++ generateFileSystemShowAux (pFileSystList) ++ "-----------------------------Files System Ends-----------------------------------------------------------------" ++ "\n" 

generateFileSystemShowAux :: ([(String,String,Char,[String],Int,String)]) -> String
generateFileSystemShowAux ([]) = ""
generateFileSystemShowAux (tmpFS : pFileSystList) = "Path " ++ sel1 tmpFS ++ "Type: " ++ sel2 tmpFS ++ show(sel5 tmpFS) ++ "\n"++generateFileSystemShow(pFileSystList)


------------------Generate string for Directories and Files--------------------------
generateDirectoriesAndFilesShow :: ([(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)])->String
generateDirectoriesAndFilesShow([],filesList) = ""
generateDirectoriesAndFilesShow (tmp:directoriesList,filesList)
	| null (sel4 tmp) = printDirInformation(tmp) ++ "\n" ++ generateDirectoriesAndFilesShow(directoriesList,filesList)
	| otherwise = printDirInformation(tmp) ++ "\n" ++ generateFileMessage(filesList,sel4 tmp) ++ generateDirectoriesAndFilesShow(directoriesList,filesList)

generateFileMessage :: ([(String,String,String,String,String,String)],[String])->String
generateFileMessage (filesList, []) = ""
generateFileMessage (tmpFile:filesList, tmp:listToPrint)
	| tmp == sel1 tmpFile = printFileInformation(tmpFile) ++ generateFileMessage(filesList,listToPrint)
	| otherwise = generateFileMessage(filesList,listToPrint)


printDirInformation :: (String,String,[String],[String],String,String,String,String,String,Bool)->String
printDirInformation (dirName,dirPath,assocDir,assocFiles,date,time,ownership,cdDirectory,fileSystMounted, isMounted) = ownership ++ "  " ++ date ++ "  " ++ time ++ "  " ++dirName ++ dirPath++"\n"

printFileInformation :: (String,String,String,String,String,String)->String
printFileInformation (name,path,pdata,date,time,ownership) = ownership ++ "  " ++ date ++ "  " ++ time ++ "  " ++name ++ path ++ "\n"
