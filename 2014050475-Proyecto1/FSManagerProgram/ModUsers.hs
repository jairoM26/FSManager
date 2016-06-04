{-# OPTIONS_GHC -fno-warn-tabs #-}
module ModUsers
(usermod
,checkNewusergroups
,existinggroups
)
where 
import GetData
import AddGroups
import DelAssociatedUsers
import AddUsers
import Data.Tuple.Select

--Check that exist the command -g
usermod:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
usermod (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y)= (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't mod user, instruction incomplete, expected : -g primaryGroup userName")
usermod (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
     | null xs = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't mod user, instruction incomplete, expected: -g or -G" )
     | (last (xs) /= "") &&  (finduser(pUserList,last (xs))==False) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," : Couldn't  find the userName:  " ++ last (xs) )
     

     | (x=="-g")  && (length xs < 1) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't mod user, expected   UserName")--If the command was -G and is 
     --only the SecondGroupName, or only the userName, the instruction is incomplete
     | x=="-g"  && (length xs > 1)= usermodgroup (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,xs,last (xs))
     | otherwise =  usermodgroupAux (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[x]++xs)
   
    
--Check the name of primary Group
usermodgroup:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
usermodgroup (pGroupList,u:pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:userGroups,userName) 
     | x=="" = usermodgroup (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[x] ++ userGroups,userName)--if the element is empty or a space call recursively the same funtion
     
     | finduser ([u],userName) = if ((sel3 (u))==x) ==True
                      then (pGroupList ,[u]++pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't mod user, existing primary group: " ++ x) 
                      else do  
                          if (existinggroups(pGroupList,[x]))==True
                            then 
                                if ( length (userGroups)<=1)
                                    then refreshGroups (  groupmodusers (pGroupList, [(sel1 (u),sel2 (u),x, sel4 (u) )]  ++ pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[("",999,[])],[sel3 (u)],userName)  , [x],userName)
                                    else do usermodgroupAux (pGroupList, [(sel1 (u),sel2 (u),x, sel4 (u) )]  ++ pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,userGroups) 
 
                            else do (pGroupList, [u] ++ pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), " Couldn't mod user, non-existent primary group : " ++ x)
                        
        
     
     | otherwise = usermodgroup (pGroupList ,pUserList ++ [u],storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[x]++userGroups,userName)

--refresh the new associated user in the groups
refreshGroups :: (([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String),[String],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
refreshGroups (pGeneralList,pGroupList,userName)= groupaddusers ( sel1 pGeneralList, sel2 pGeneralList, sel1 (sel3 pGeneralList), sel2 (sel3 pGeneralList),sel3 (sel3 pGeneralList),sel1 (sel4 pGeneralList),sel2 (sel4 pGeneralList),sel3 (sel4 pGeneralList),[("",999,[])],pGroupList,userName)

usermodgroupAux :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]])->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
usermodgroupAux (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[]) = (pGroupList,pUserList, (storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Error mod user, args missings")
usermodgroupAux (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs)
	
    | (x=="-G")   && null xs = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't mod user, expected   SeconfGroupName")--If the command was -G and the 
     --list is empty, the instruction is incomplete
	| (x=="-G")  && (length xs < 1) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't mod user, expected   UserName")--If the command was -G and is 
     --only the SecondGroupName, or only the userName, the instruction is incomplete
    | (x == "-G") && (length xs > 1) = usermodSecondGroup(pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, init (xs), last (xs))
    
    | otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't find: " ++ x)--If the group doesn't exist , can't add the user


usermodSecondGroup:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[String],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
usermodSecondGroup (pGroupList,u:pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,userGroups,userName) 
    

     | finduser ([u],userName) = if (checkNewusergroups(sel4 (u),userGroups)) ==True
                      then (pGroupList ,[u]++pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Couldn't add the secondary groups in user:  existing   groups") 
                      else do  
                          if (existinggroups(pGroupList,userGroups))==True
                            then groupaddusers (pGroupList, [(sel1 (u),sel2 (u),sel3 (u), sel4 (u) ++ (userGroups) )]  ++ pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[("",999,[])],userGroups,userName)
                            else do (pGroupList, [u] ++ pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), " Couldn't add the secondary groups in user: non-existent groups")
                        
        
     | otherwise = usermodSecondGroup (pGroupList ,pUserList ++ [u],storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,userGroups,userName)

--Check if the new groups exist in the user: True,Exist. False,Unexist
checkNewusergroups :: ([String],[String]) -> Bool
checkNewusergroups (pUserList,[]) = False
checkNewusergroups (pUserList,x:userGroups)
     | x `elem` pUserList= True
     | otherwise = checkNewusergroups(pUserList,userGroups)

--Check if the new groups don't exist : True,Exist. False,Unexist
existinggroups :: ([(String,Int,[String])],[String]) -> Bool
existinggroups (pGroupList,[]) = True
existinggroups (pGroupList,x:userGroups)
     | findgroup(pGroupList,x)= existinggroups(pGroupList,userGroups)
     | otherwise = False