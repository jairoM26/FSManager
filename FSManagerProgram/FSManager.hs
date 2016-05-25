-- @author: Jairo Méndez 
-- @author: Cristian Castillo
-- @date: 18 - 05 -2016
-- @File: FSManager.hs
-- @brief: A virtual file system manager

import Control.Monad  -- library that control the secuense of the program, like the command forever
import Data.List.Split-- library split

-----------------------------------------------------------------------------------------------------------------
									-- IMPORT MY DATA STRUCTURE --
-----------------------------------------------------------------------------------------------------------------
import LinkedList
-----------------------------------------------------------------------------------------------------------------
									-- IMPORT MY GET DATA FUNCTIONS --
-----------------------------------------------------------------------------------------------------------------
import GetData

-----------------------------------------------------------------------------------------------------------------
									-- IMPLEMENTING FSMANAGER LOGIC --
-----------------------------------------------------------------------------------------------------------------

{-
     Group Info = (groupName, GroupID, UserAssocList)   
     UserInfo = (userName, UID, primaryGroup, SecondGroupList) 
-}

{-
 --Main functions
 --Calls tha principal function with initial parameters
-}
--Infinite loop
main ::  IO()
main =  do 
     fsManager ([],[]," ")
     
    
{-
  --FSMANAGER FORMAT
     fsManager (groupList, userList, messageToprint)
  --This function take the instructions from the FSManagerConsole (interfaz) and starts calling function that will modify the list according
    to the input instruction
  --This functions calls itself recursively with the Total parameteres modified
-}
fsManager :: ([(String,Int,[String])],[(String,Int,String,[String])],String)-> IO()
fsManager pGeneralList = do
     
     putStrLn (tupleGetThird' pGeneralList)
     putStr ( "root$root: ")
     inputInstruction <- getLine --Get the input instrction
     {-
        --Send the input instruction to another function who will decodify and verify the input, and the apply the correct action
        --splitOn " " inputInstruction generate a list, make a split in every " " in the input
        --tupleGetFirst' pGeneralList = List of groups
        --tupleGetSecond' pGeneralList = List of users
     -}
     fsManager (input (tupleGetFirst' pGeneralList, tupleGetSecond' pGeneralList, (splitOn  " " inputInstruction),inputInstruction))
     

    
--Check what is the first command
--According to the first command calls function that will apply that command
{-
   -- ([(String,Int,[String])],[[Char]],String) = pGroupList, CommandList, Command    
-}
input:: ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
input (pGroupList,pUserList,[],y)= (pGroupList,pUserList,"")
input (pGroupList,pUserList,x:xs,y)
     | x == "" = input (pGroupList,pUserList,xs,y)
     | x=="groupadd"  = groupadd (pGroupList,pUserList,xs,y)
     | x=="useradd"= useradd (pGroupList,pUserList,xs,y)
     | x=="show"= showatributes (pGroupList,pUserList,xs,y)
     | otherwise = (pGroupList,pUserList, "Couldn't  find: " ++ x)
{-
     
     
     | x=="finger" = showuseraccount (xs,y)
     | x=="userdel" = userdel (xs,y)
     | x=="groupdel" = groupdel (xs,y)
-}

--Check that exist the command -g
usermod::  ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
usermod (pGroupList,pUserList,[],y)=  (pGroupList,pUserList," Couldn't modify user,  instruction incomplete,expected : [-g primaryGroup] [-G secondaryGroup1, secondaryGroup2] userName")
usermod (pGroupList,pUserList,x:xs,y)
     | x=="" = usermod (pGroupList,pUserList,xs,y) --if the element is empty or a space call recursively the same funtion
     | x=="-g" = useraddgroup (pGroupList,pUserList,xs,y)
     | otherwise =  (pGroupList,pUserList,y ++ " : Couldn't create user, expected -g  " )

--Remove  users
userdel:: ([[Char]],String) -> String
userdel ([],y) = "Couldn't remove the user , instruction incomplete,expected: userName"
userdel (x:xs,y)
     | x=="" = userdel (xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = " Couldn't remove the user with: " ++ y -- If after the command are more information o text, 
     --the syntax is incorrect
     {-| finduser x =  "Removing  userName: " ++ x  -- If the user exist,  it remove the user-}
     | otherwise =  " Couldn't   find the userName: " ++ x -- If the user doesn't exist

--Remove  groups
groupdel:: ([[Char]],String) -> String
groupdel ([],y) = "Couldn't remove the  group , instruction incomplete,: expected groupName"
groupdel (x:xs,y)
     | x=="" = userdel (xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = " Couldn't remove the group with: " ++ y -- If after the command are more information o text, 
     --the syntax is incorrect
     {-| findgroup x =  "Removing  groupName: " ++ x  -- If the group exist, it remove the group-}
     |  otherwise =  " Couldn't   find the groupName: " ++ x -- If the group doesn't exist


{-
  --Function that show the information about groups or user   
-}
showatributes:: ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
showatributes (pGroupList,pUserList,[],y) = (pGroupList,pUserList,"Couldn't show, instruction incomplete,expected: users or groups")
showatributes (pGroupList,pUserList,x:xs,y) 
     | x=="" = showatributes (pGroupList,pUserList, xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = (pGroupList,pUserList,	 " Couldn't show information with: " ++ y) -- If after the command are more information o text, 
     --the syntax is incorrect
     {-| x=="users" = showusers-}
     | x=="groups" = showgroups (pGroupList,pUserList)

     | otherwise = (pGroupList,pUserList," Couldn't find:  " ++ x) --If the group exist doesn't add the new group

--The following  funtion will be used to  show all the existent user groups in the
--system with their corresponding attributes
showgroups:: ([(String,Int,[String])],[(String,Int,String,[String])])->([(String,Int,[String])],[(String,Int,String,[String])],String)
showgroups (pGroupList,pUserList) = showgroupsAux (pGroupList,pGroupList,pUserList,"GroupName                                  GID           AssociatedUsers " ++ "\n")

showgroupsAux :: ([(String,Int,[String])],[(String,Int,[String])],[(String,Int,String,[String])],String)->([(String,Int,[String])],[(String,Int,String,[String])],String)
showgroupsAux  ([],pList,pUserList,pMessage) = (pList,pUserList,pMessage)
showgroupsAux (x:pGroupList,pList, pUserList, pMessage )= showgroupsAux (pGroupList,pList, pUserList, pMessage ++ tupleGetFirst' x ++ addspaces( 42- length(tupleGetFirst' x),"") ++ show(tupleGetSecond' x) ++ addspaces( 10,"") ++ generateAssocUserGroupString (tupleGetThird' x) ++  "\n")


{-}
--The following  funtion will be used to  show all the existent user users in the
--system with their corresponding attributes
showusers:: ([(String,Int,[String])],[(String,Int,String,[String])])->([(String,Int,[String])],[(String,Int,String,[String])],String)
showusers (pGroupList,pUserList) = 
-}
--The following  funtion will be used to  show the details of a specific user account
showuseraccount:: ([[Char]],String) ->String
showuseraccount ([],y) = " Couldn't show the user account, instruction incomplete, expected: name"
showuseraccount (x:xs,y)
     | x=="" = showuseraccount (xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = " Couldn't  show the user account with: " ++ y -- If after the name are more information o text, 
     --the syntax is incorrect
     {-| finduser x =  "Showing User Account" -- If the user exist, show its atributes-}
     |  otherwise =  " Couldn't   find the user account: " ++ x -- If the user doesn't exist


--Add a new group
groupadd:: ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
groupadd (pGroupList,pUserList,[],y) = (pGroupList,pUserList," Couldn't create group, expected: groupName")
groupadd (pGroupList,pUserList,x:xs,y)
     | x=="" = groupadd (pGroupList,pUserList,xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = (pGroupList,pUserList," Couldn't create group with: " ++ y) -- If after the name are more information o text, 
     --the syntax is incorrect
     | findgroup (pGroupList,x) = (pGroupList,pUserList," Couldn't add, existing group  " )--If the group exist doesn't add the new group
     | otherwise = if (length pGroupList) == 0
                    then (pGroupList ++ [(x, 1000, [])],pUserList,"  group added: " ++ "  groupName: "  ++ x)
                    else do (pGroupList ++ [(x, (tupleGetSecond' (last pGroupList)) + 1, [])],pUserList,"  group added: " ++ "  groupName: "  ++ x)--If the group doesn't exist add the  new group

--Check that exist the command -g
useradd:: ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
useradd (pGroupList,pUserList,[],y)= (pGroupList,pUserList," Couldn't create user, instruction incomplete, expected : -g primaryGroup userName")
useradd (pGroupList,pUserList,x:xs,y)
     | x=="" = useradd (pGroupList,pUserList,xs,y) --if the element is empty or a space call recursively the same funtion
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
	| x=="" = useraddgroupAux (pGroupList,pUserList,userToAdd,xs) --if the element is empty or a space call recursively the same funtion
    | (x=="-G")  && null xs = (pGroupList,pUserList," Couldn't create user, expected   SeconfGroupName")--If the command was -G and the 
     --list is empty, the instruction is incomplete
	| (x=="-G")  && (length xs < 1) = (pGroupList,pUserList," Couldn't create user, expected   UserName")--If the command was -G and is 
     --only the SecondGroupName, or only the userName, the instruction is incomplete
    | (x == "-G") && (length xs > 1) = useraddSecondGroup(pGroupList,pUserList,userToAdd,xs)
    | null xs = addUserName (pGroupList,pUserList,userToAdd,[x])--If are one element in the list, that element is the UserName
    | otherwise = (pGroupList,pUserList,"Couldn't find: " ++ x)--If the group doesn't exist , can't add the user

useraddSecondGroup :: ([(String,Int,[String])],[(String,Int,String,[String])],(String,Int,String,[String]),[[Char]])-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
useraddSecondGroup (pGroupList,pUserList,userToAdd,[]) = (pGroupList,pUserList, "Error created user, args missings")
useraddSecondGroup (pGroupList,pUserList,userToAdd,x:xs)
	| x=="" = useraddSecondGroup (pGroupList,pUserList,userToAdd,xs) --if the element is empty or a space call recursively the same funtion
	| length xs == 1 = addUserName (pGroupList,pUserList,userToAdd,xs)
	| findgroup (pGroupList, x) = useraddSecondGroup(pGroupList,pUserList, ("",999, tupleGetThird userToAdd, (tupleGetFourth userToAdd) ++ [x]), xs)
	| otherwise =  (pGroupList,pUserList,"Couldn't find: " ++ x)--If the group doesn't exist so can't add the user

addUserName :: ([(String,Int,[String])],[(String,Int,String,[String])],(String,Int,String,[String]),[[Char]])-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
addUserName (pGroupList,pUserList,userToAdd,x:xs)
	| finduser (pUserList, x) = (pGroupList,pUserList," Couldn't add, existing User")-- If the user exist, doesn't add the new user
	| otherwise = if (length pUserList) == 0
     				then  (pGroupList,pUserList ++ [((tupleGetFirst userToAdd) ++ x, 1000, tupleGetThird userToAdd, tupleGetFourth userToAdd)],"User added: " ++ "UserName: " ++ x) -- If the user doesn't exist, add the ner user
					else do  (pGroupList,pUserList ++ [((tupleGetFirst userToAdd) ++ x, (tupleGetSecond (last pUserList)) + 1, tupleGetThird userToAdd, tupleGetFourth userToAdd)],"User added: " ++ "UserName: " ++ x) -- If the user doesn't exist, add the ner user
     				--else do (pGroupList,pUserList ++ [((tupleGetFirst userToAdd) ++ x, (tupleGetSecond' (last pUserList)) + 1, tupleGetThird userToAdd, tupleGetFourth userToAdd)],"User added: " ++ "UserName: " ++ x)

{-
addusersToGroups :: ([(String,Int,[String])], [(String,Int,String,[String])],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
addusersToGroups -}
--Check if the user exist
finduser:: ([(String,Int,String,[String])],String) -> Bool
finduser (pGroupList, x)
     | null pGroupList = False
     | (x `elem` (generateNamesUserList pGroupList)) =  True
     | otherwise = False

---Check if the group exist
findgroup:: ([(String,Int,[String])],String) -> Bool
findgroup (pGroupList, x)
     | null pGroupList = False
     | (x `elem` (generateNamesGroupList pGroupList)) =  True
     | otherwise = False