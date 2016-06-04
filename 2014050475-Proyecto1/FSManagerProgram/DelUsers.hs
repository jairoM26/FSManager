{-# OPTIONS_GHC -fno-warn-tabs #-}
module DelUsers
(
deluser
)
where 
import GetData
import AddGroups
import AddUsers
import DelAssociatedUsers
import Data.Tuple.Select

--Remove the userName
deluser :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]])->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
deluser (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[])  = ( pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't remove the user, expected userName" )

deluser (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,userName:xs) 
     | finduser(pUserList,userName) =  groupmodusers (pGroupList,deluserAux(pUserList,userName),storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, [("",999,[])],getusergroups (pUserList,userName),userName)
                  
  
     |    otherwise =  ( pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't find, non-existent user : " ++ userName) 



--Return the list users without one  user, in this case userName, 
deluserAux :: ([(String,Int,String,[String])],String)-> [(String,Int,String,[String])]
deluserAux ([], userName) = []
deluserAux (x:pUsersList, userName)
     |    sel1 (x)==userName =   deluserAux(pUsersList,userName)
     |    otherwise = [x] ++ deluserAux(pUsersList,userName)

--Return the list  assciated groups of one user, in this case the userName
getusergroups:: ([(String,Int,String,[String])],String)-> [String]
getusergroups ([], userName) = []
getusergroups (x:pUsersList, userName)
     |    sel1 (x)==userName =   [sel3 (x)] ++   sel4 (x)
     |    otherwise = getusergroups(pUsersList,userName)


