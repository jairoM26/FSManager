module DelAssociatedUsers
(groupmodusers
,groupdelusers

)
where 
import GetData
import AddGroups
import AddUsers
import Data.Tuple.Select

--Remove an associates user in the list of groups
groupmodusers:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[(String,Int,[String])],[String],String)-> ([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
groupmodusers (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,newpGroupsList,[],userName) =  ( newpGroupsList ++ pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Removed user : " ++ userName) 

groupmodusers (g:pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,newpGroupsList,x:userGroups,userName) 

     | findgroup ([g],x) =  if sel1 (head (newpGroupsList)) == ""
                    then groupmodusers (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, [(sel1 (g),sel2 (g),groupdelusers (sel3 (g) ,userName) )],userGroups,userName)
                    else do groupmodusers (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, newpGroupsList ++ [(sel1 (g),sel2 (g),groupdelusers (sel3 (g) ,userName) )] ,userGroups,userName)

     	
     | otherwise = groupmodusers (pGroupList ++ [g],pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,   newpGroupsList ,[x]++userGroups,userName)

--Return a list of associated users without one  user, in this case is userName
groupdelusers :: ([String],String)-> [String]
groupdelusers ([], userName) = []
groupdelusers (x:pGroupUsers, userName)
     |    x==userName = groupdelusers( pGroupUsers,userName)
     |    otherwise = [x] ++ groupdelusers(pGroupUsers,userName)