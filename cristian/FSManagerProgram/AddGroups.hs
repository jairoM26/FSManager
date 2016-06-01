{-# OPTIONS_GHC -fno-warn-tabs #-}
module AddGroups
(groupadd
, findgroup
,groupaddusers
,getgroup
)
where 
import GetData
import Data.Tuple.Select

--Add a new group
groupadd:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
groupadd (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't create group, expected: groupName")
groupadd (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
     
     | (length xs>=1) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't create group with: " ++ y) -- If after the name are more information o text, 
     --the syntax is incorrect
     | findgroup (pGroupList,x) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't add, existing group  " )--If the group exist doesn't add the new group
     | otherwise = if (length pGroupList) == 0
                    then (pGroupList ++ [(x, 1000, [])],pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Added group : " ++ x)
                    else do (pGroupList ++ [(x, (tupleGetSecond' (last pGroupList)) + 1, [])],pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Added group : " ++ x)--If the group doesn't exist add the  new group

--add the users  in  each group
--userGroups: all names of the groups
groupaddusers:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[(String,Int,[String])],[String],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
groupaddusers (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,newpGroupsList,[],userName) =  ( newpGroupsList ++ pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," updated user : " ++ userName ++ " and  groups") 

groupaddusers (g:pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,newpGroupsList,x:userGroups,userName) 

     | findgroup ([g],x) =  if sel1 (head (newpGroupsList)) == ""
                    then groupaddusers (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, [(sel1 (g),sel2 (g),sel3 (g) ++ [userName])]  ,userGroups,userName)
                    else do groupaddusers (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, newpGroupsList ++ [(sel1 (g),sel2 (g),sel3 (g) ++ [userName])]  ,userGroups,userName)

     	
     | otherwise = groupaddusers (pGroupList ++ [g],pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,   newpGroupsList ,[x]++userGroups,userName)



--Get the a group
getgroup:: ([(String,Int,[String])],String)->[(String,Int,[String])]
getgroup (x:groups,usergroup)

     | sel1 (x)==usergroup = [x]
     |otherwise = getgroup (groups,usergroup)
---Check if the group exist
findgroup:: ([(String,Int,[String])],String) -> Bool
findgroup (pGroupList, x)
     | null pGroupList = False
     | (x `elem` (generateNamesGroupList pGroupList)) =  True
     | otherwise = False                   