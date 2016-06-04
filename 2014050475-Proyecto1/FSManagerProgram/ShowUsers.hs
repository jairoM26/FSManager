{-# OPTIONS_GHC -fno-warn-tabs #-}
module ShowUsers
(showUsers
, finger
)
where 
import GetData
import AddUsers
import Data.Tuple.Select

--The following  funtion will be used to  show all the existent user groups in the
--system with their corresponding attributes
showUsers:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)])->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
showUsers (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList) = showUsersAux (pGroupList,pUserList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,"UserName                            UID           primaryGroup                        SecondaryGroup                      HomeDirectory" ++ "\n")

showUsersAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
showUsersAux  (pGroupList,[],pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),pMessage)
showUsersAux (pGroupList,x:pList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage) = showUsersAux (pGroupList,pList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage ++ tupleGetFirst x ++ addspaces(36- length(tupleGetFirst x),"") ++ show(tupleGetSecond x)   ++ addspaces( 10,"") ++ (tupleGetThird x) ++ addspaces( 36- length(tupleGetThird x),"") ++ generateSecondaryGroupNames (tupleGetFourth x)++ addspaces( 36- length(generateSecondaryGroupNames (tupleGetFourth x)),"") ++ "/home/" ++ tupleGetFirst x ++ "\n")

finger :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]])->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
finger (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[]) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't show finger, expected: user name")
finger (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs)
	| finduser (pUserList,x) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), printUserData (getUser (pUserList,x)))
	| otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't do finger, username: " ++ x ++ "does not exist")


printUserData :: (String,Int,String,[String])->String
printUserData user = "User Name: " ++ (sel1 user) ++ "\n" ++ "UID: " ++ show(sel2 user) ++ addspaces(10," ") ++ "Home Directory: " ++ "home/" ++ (sel1 user) ++ "\n" ++ 
					"Associated primary group: " ++ (sel3 user) ++ "\n" ++ "Associated secundary groups: " ++ "\n" ++ show (sel4 user)

getUser :: ([(String,Int,String,[String])],String)->(String,Int,String,[String])
getUser (tmp:pUserList,userName)
	| userName == (sel1 tmp) = tmp
	|otherwise = getUser(pUserList,userName)