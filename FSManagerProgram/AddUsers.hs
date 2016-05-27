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

--Check that exist the command -g
useradd:: ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
useradd (pGroupList,pUserList,[],y)= (pGroupList,pUserList," Couldn't create user, instruction incomplete, expected : -g primaryGroup userName")
useradd (pGroupList,pUserList,x:xs,y)
    
     | x=="-g" = useraddgroup (pGroupList,pUserList,xs,y)
     | otherwise =  (pGroupList,pUserList,y ++ " : Couldn't create user, expected -g  " )
   
    
--Check the name of primary Group
useraddgroup:: ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
useraddgroup (pGroupList,pUserList,[],y) =  (pGroupList,pUserList," Couldn't create user, instruction incomplete, expected: name primary group and UserName")
useraddgroup (pGroupList,pUserList,x:xs,y)
     | x=="" = useraddgroup (pGroupList,pUserList,xs,y)--if the element is empty or a space call recursively the same funtion
     | findgroup (pGroupList,x) =  useraddgroupAux (pGroupList,pUserList,("",999,x,[]),xs)--If the group exist so, add the group and call the funtion 
     --useradd_secondgroup to check the next command
     | otherwise =  (pGroupList,pUserList,"Couldn't find: " ++ x)--If the group doesn't exist so can't add the user

useraddgroupAux :: ([(String,Int,[String])],[(String,Int,String,[String])],(String,Int,String,[String]),[[Char]])-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
useraddgroupAux (pGroupList,pUserList,userToAdd,[]) = (pGroupList,pUserList, "Error created user, args missings")
useraddgroupAux (pGroupList,pUserList,userToAdd,x:xs)
	
    | (x=="-G")   && null xs = (pGroupList,pUserList," Couldn't create user, expected   SeconfGroupName")--If the command was -G and the 
     --list is empty, the instruction is incomplete
	| (x=="-G")  && (length xs < 1) = (pGroupList,pUserList," Couldn't create user, expected   UserName")--If the command was -G and is 
     --only the SecondGroupName, or only the userName, the instruction is incomplete
    | (x == "-G") && (length xs > 1) = useraddSecondGroup(pGroupList,pUserList,userToAdd,xs)
    | null xs = addUserName (pGroupList,pUserList,userToAdd,x)--If are one element in the list, that element is the UserName
    | otherwise = (pGroupList,pUserList,"Couldn't find: " ++ x)--If the group doesn't exist , can't add the user

useraddSecondGroup :: ([(String,Int,[String])],[(String,Int,String,[String])],(String,Int,String,[String]),[[Char]])-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
useraddSecondGroup (pGroupList,pUserList,userToAdd,[]) = (pGroupList,pUserList, "Error created user, args missings")
useraddSecondGroup (pGroupList,pUserList,userToAdd,x:xs)
	
	| null xs  = addUserName (pGroupList,pUserList,userToAdd,x)
	| findgroup (pGroupList, x) = useraddSecondGroup(pGroupList,pUserList, ("",999, tupleGetThird userToAdd, (tupleGetFourth userToAdd) ++ [x]), xs)
	| otherwise =  (pGroupList,pUserList,"Couldn't find: " ++ x)--If the group doesn't exist so can't add the user

addUserName :: ([(String,Int,[String])],[(String,Int,String,[String])],(String,Int,String,[String]),String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
addUserName (pGroupList,pUserList,userToAdd,x)
	| finduser (pUserList, x) = (pGroupList,pUserList," Couldn't add, existing User")-- If the user exist, doesn't add the new user
	| otherwise = if (length pUserList) == 0
     				then  (pGroupList,pUserList ++ [((tupleGetFirst userToAdd) ++ x, 1000, tupleGetThird userToAdd, tupleGetFourth userToAdd)],
     						"") -- If the user doesn't exist, add the ner user
					else do  (pGroupList,pUserList ++ [((tupleGetFirst userToAdd) ++ x, (tupleGetSecond (last pUserList)) + 1, tupleGetThird userToAdd, tupleGetFourth userToAdd)],"") -- If the user doesn't exist, add the ner user
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