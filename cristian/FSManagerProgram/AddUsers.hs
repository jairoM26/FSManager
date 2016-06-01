{-# OPTIONS_GHC -fno-warn-tabs #-}
module AddUsers
(useradd
,useraddgroup
,useraddgroupAux
,useraddSecondGroup
,addUserName
,finduser
)
where 
import GetData
import AddGroups
import Data.Tuple.Select

--Check that exist the command -g
useradd:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
useradd (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y)= (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't create user, instruction incomplete, expected : -g primaryGroup userName")
useradd (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
    
     | x=="-g" = useraddgroup (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
     | otherwise =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),y ++ " : Couldn't create user, expected -g  " )
   
    
--Check the name of primary Group
useraddgroup:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
useraddgroup (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y) =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't create user, instruction incomplete, expected: name primary group and UserName")
useraddgroup (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
     | x=="" = useraddgroup (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)--if the element is empty or a space call recursively the same funtion
     | findgroup (pGroupList,x) =  useraddgroupAux (pGroupList,pUserList,("",999,x,[]),storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,xs)--If the group exist so, add the group and call the funtion 
     --useradd_secondgroup to check the next command
     | otherwise =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't find: " ++ x)--If the group doesn't exist so can't add the user

useraddgroupAux :: ([(String,Int,[String])],[(String,Int,String,[String])],(String,Int,String,[String]),[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]])->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
useraddgroupAux (pGroupList,pUserList,userToAdd,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[]) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Error created user, args missings")
useraddgroupAux (pGroupList,pUserList,userToAdd,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs)
	
    | (x=="-G")   && null xs = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't create user, expected   SeconfGroupName")--If the command was -G and the 
     --list is empty, the instruction is incomplete
	| (x=="-G")  && (length xs < 1) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't create user, expected   UserName")--If the command was -G and is 
     --only the SecondGroupName, or only the userName, the instruction is incomplete
    | (x == "-G") && (length xs > 1) = useraddSecondGroup(pGroupList,pUserList,userToAdd,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,xs)
    | null xs = addUserName (pGroupList,pUserList,userToAdd,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x)--If are one element in the list, that element is the UserName
    | otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't find: " ++ x)--If the group doesn't exist , can't add the user

useraddSecondGroup :: ([(String,Int,[String])],[(String,Int,String,[String])],(String,Int,String,[String]),[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]])->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
useraddSecondGroup (pGroupList,pUserList,userToAdd,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[]) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "Error created user, args missings")
useraddSecondGroup (pGroupList,pUserList,userToAdd,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs)
	
	| null xs  = addUserName (pGroupList,pUserList,userToAdd,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x)
	| findgroup (pGroupList, x) = useraddSecondGroup(pGroupList,pUserList, ("",999, tupleGetThird userToAdd, (tupleGetFourth userToAdd) ++ [x]),storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, xs)
	| otherwise =  (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't find: " ++ x)--If the group doesn't exist so can't add the user

addUserName :: ([(String,Int,[String])],[(String,Int,String,[String])],(String,Int,String,[String]),[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
addUserName (pGroupList,pUserList,userToAdd,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x)
	| finduser (pUserList, x) = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList)," Couldn't add, existing User")-- If the user exist, doesn't add the new user
	| otherwise = if (length pUserList) == 0
     				then  groupaddusers (pGroupList,pUserList ++ [((tupleGetFirst userToAdd) ++ x, 1000, tupleGetThird userToAdd, tupleGetFourth userToAdd)],storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[("",999,[])],
     						[sel3 (userToAdd)] ++ (sel4 (userToAdd)),x) -- If the user doesn't exist, add the ner user
					else do  groupaddusers (pGroupList,pUserList ++ [((tupleGetFirst userToAdd) ++ x, (tupleGetSecond (last pUserList)) + 1, tupleGetThird userToAdd, tupleGetFourth userToAdd)],storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[("",999,[])],[sel3 (userToAdd)] ++ (sel4 (userToAdd)),x) -- If the user doesn't exist, add the ner user
     				--else do (pGroupList,pUserList ++ [((tupleGetFirst userToAdd) ++ x, (tupleGetSecond' (last pUserList)) + 1, tupleGetThird userToAdd, tupleGetFourth userToAdd)],"User added: " ++ "UserName: " ++ x)

{-
addusersToGroups :: ([(String,Int,[String])], [(String,Int,String,[String])],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
addusersToGroups -}

{-
--The following  funtion will be used to  show the details of a specific user account
showuseraccount::([(String,Int,[String])],[(String,Int,String, [String])],[(String,Int,String,[String])],String,[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],String)
showuseraccount (pGroupList,x:pList, pUserList, pMessage ,[],y) = " Couldn't show the user account, instruction incomplete, expected: name"
showuseraccount  (pGroupList,[],pUserList,pMessage,userName,y) = (pGroupList,pUserList,pMessage)
showuseraccount (pGroupList,x:pList, pUserList, pMessage ,usn:userName,y)
	| (length userName>=1) = (pGroupList,pUserList,pMessage++" Couldn't  show the user account with: " ++ y) -- If after the name are more information o text, --the syntax is incorrect 
	| (finduser(pUserList,usn)==False) =   showuseraccount(pGroupList,pList, pUserList, pMessage ++" Couldn't   find the user account: " ++ x ,[usn],y)-- If the user doesn't exist
	| otherwise= showuseraccount (pGroupList,pList, pUserList, pMessage ++ tupleGetFirst x ++ addspaces(36- length(tupleGetFirst x),"") ++ show(tupleGetSecond x)   ++ addspaces( 10,"") ++ (tupleGetThird x) ++ addspaces( 36- length(tupleGetThird x),"") ++ generateSecondaryGroupNames (tupleGetFourth x)  ++ addspaces( 36- length(generateSecondaryGroupNames (tupleGetFourth x)),"") ++ "/home/" ++ tupleGetFirst x ++ "\n",[usn],y)
-}

--Check if the user exist
finduser:: ([(String,Int,String,[String])],String) -> Bool
finduser (pUserList, x)
     | null pUserList = False
     | (x `elem` (generateNamesUserList pUserList)) =  True
     | otherwise = False