{-# OPTIONS_GHC -fno-warn-tabs #-}
module DelGroups
(
delgroup
)
where 
import GetData
import AddGroups
import AddUsers
import DelAssociatedGroups
import Data.Tuple.Select

--Remove the groupName
delgroup ::  ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]])->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)

delgroup (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[])  = ( pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't remove the group, expected groupName" )
delgroup (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,groupName:xs) 
     | ((null groupName == False) && (checkgroupusers(pUserList,groupName))) = ( pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't  remove the group, because : " ++ groupName ++ " , is a primary group") 
     | findgroup(pGroupList,groupName) =  usermodgroups ( delgroupAux(pGroupList,groupName),pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, [("",999,"",[])],getgroupusers (pGroupList,groupName),groupName)
                  
  
     |    otherwise =  ( pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't find, non-existent group : " ++ groupName) 



--Return the list groups without one  group, in this case groupName, 
delgroupAux :: ([(String,Int,[String])],String)-> [(String,Int,[String])]
delgroupAux ([], groupName) = []
delgroupAux (x:pGroupsList, groupName)
     |    sel1 (x)==groupName =   delgroupAux(pGroupsList,groupName)
     |    otherwise = [x] ++ delgroupAux(pGroupsList,groupName)

--Return the list  assciated users of one group, in this case the groupName
getgroupusers:: ([(String,Int,[String])],String)-> [String]
getgroupusers ([], groupName) = []
getgroupusers (x:pGroupsList, groupName)
     |    sel1 (x)==groupName =   sel3 (x)
     |    otherwise = getgroupusers(pGroupsList,groupName)

--Check if there are one user that have the groupName like primary group, True: if are a primary group, False : if not
checkgroupusers :: ([(String,Int,String,[String])],String) -> Bool
checkgroupusers ([],groupName)  = False
checkgroupusers (x:pUsersList,groupName)
     | sel3 (x) == groupName = True
     | otherwise = checkgroupusers(pUsersList,groupName) 


