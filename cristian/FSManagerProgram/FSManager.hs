{-# OPTIONS_GHC -fno-warn-tabs #-}
-- @author: Jairo Méndez 
-- @author: Cristian Castillo
-- @date: 18 - 05 -2016
-- @File: FSManager.hs
-- @brief: A virtual file system manager

import Control.Monad  -- library that control the secuense of the program, like the command forever
import Data.List.Split-- library split
import ShowGroups
import ShowUsers
import AddGroups
import AddUsers

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
     --putStrLn (show (splitOn "/" inputInstruction))
     {-
        --Send the input instruction to another function who will decodify and verify the input, and the apply the correct action
        --splitOn " " inputInstruction generate a list, make a split in every " " in the input
        --tupleGetFirst' pGeneralList = List of groups
        --tupleGetSecond' pGeneralList = List of users
     -}
     fsManager (input (tupleGetFirst' pGeneralList, tupleGetSecond' pGeneralList, (split (dropDelims $ oneOf " ") inputInstruction),inputInstruction))
        
--Check what is the first command
--According to the first command calls function that will apply that command
{-
   -- ([(String,Int,[String])],[[Char]],String) = pGroupList, CommandList, Command    
-}
input:: ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
input (pGroupList,pUserList,[],y)= (pGroupList,pUserList,"")
input (pGroupList,pUserList,x:xs,y)
      | x=="groupadd"  = groupadd (pGroupList,pUserList,xs,y)
      | x=="useradd"= useradd (pGroupList,pUserList,xs,y)
      | x=="show"= showatributes (pGroupList,pUserList,xs,y)
      {-| x=="finger" = showUsersAux (pGroupList,pUserList,pUserList,"",xs)-}
      | otherwise = (pGroupList,pUserList, "Couldn't  find: " ++ x)

--Check that exist the command -g
usermod::  ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
usermod (pGroupList,pUserList,[],y)=  (pGroupList,pUserList," Couldn't modify user,  instruction incomplete,expected : [-g primaryGroup] [-G secondaryGroup1, secondaryGroup2] userName")
usermod (pGroupList,pUserList,x:xs,y)
     
     | x=="-g" = useraddgroup (pGroupList,pUserList,xs,y)
     | otherwise =  (pGroupList,pUserList,y ++ " : Couldn't create user, expected -g  " )

--Remove  users
userdel:: ([[Char]],String) -> String
userdel ([],y) = "Couldn't remove the user , instruction incomplete,expected: userName"
userdel (x:xs,y)
 
     | (length xs>=1) = " Couldn't remove the user with: " ++ y -- If after the command are more information o text, 
     --the syntax is incorrect
     {-| finduser x =  "Removing  userName: " ++ x  -- If the user exist,  it remove the user-}
     | otherwise =  " Couldn't   find the userName: " ++ x -- If the user doesn't exist

--Remove  groups
groupdel:: ([[Char]],String) -> String
groupdel ([],y) = "Couldn't remove the  group , instruction incomplete,: expected groupName"
groupdel (x:xs,y)
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
   
     | (length xs>=1) = (pGroupList,pUserList,	 " Couldn't show information with: " ++ y) -- If after the command are more information o text, 
     --the syntax is incorrect
     | x=="users" = showUsers (pGroupList,pUserList)
     | x=="groups" = showgroups (pGroupList,pUserList)

     | otherwise = (pGroupList,pUserList," Couldn't find:  " ++ x) --If the group exist doesn't add the new group