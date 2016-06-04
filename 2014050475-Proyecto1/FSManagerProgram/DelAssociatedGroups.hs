{-# OPTIONS_GHC -fno-warn-tabs #-}
module DelAssociatedGroups
(
usermodgroups
,userdelgroups
)
where 
import GetData
import AddGroups
import AddUsers
import Data.Tuple.Select

--Remove an associates group
usermodgroups:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[(String,Int,String,[String])],[String],String)-> ([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
usermodgroups (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,newpUsersList,[],groupName) =  ( pGroupList,newpUsersList ++ pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Removed group : " ++ groupName) 

usermodgroups (pGroupList,u:pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,newpUsersList,x:groupsUsers,groupName) 

     | finduser ([u],x) =  if sel1 (head (newpUsersList)) == ""
                    then usermodgroups (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, [(sel1 (u),sel2 (u),sel3 (u),userdelgroups (sel4 (u) ,groupName) )],groupsUsers,groupName)
                    else do usermodgroups (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, newpUsersList ++  [(sel1 (u),sel2 (u),sel3 (u),userdelgroups (sel4 (u) ,groupName) )] ,groupsUsers,groupName)

     	
     | otherwise = usermodgroups (pGroupList ,pUserList ++ [u],storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,   newpUsersList ,[x]++groupsUsers,groupName)

--Return a list of associated groups without one  group, in this case is groupName
userdelgroups :: ([String],String)-> [String]
userdelgroups ([], groupName) = []
userdelgroups (x:pGroupUsers, groupName)
     |    x==groupName = userdelgroups( pGroupUsers,groupName)
     |    otherwise = [x] ++ userdelgroups(pGroupUsers,groupName)