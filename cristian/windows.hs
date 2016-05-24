import Control.Monad  -- library that control the secuense of the program, like the command forever
import Data.List.Split-- library split
{-
 -- @author: Cristian Castillo 
 -- @date: 22 - 05 - 2016
 -- @brief: That algorithm checks the commands are valid
-}
--Infinite loop
main = forever $ do  
	
    putStr "# "  
    l <- getLine  
    
    putStrLn (input   ((splitOn  " " l),l))--Make a list of the input tex and split the spaces

--Check what is the first command
input::  ([[Char]],String)-> String
input ([],y)= ""
input (x:xs,y)

     | x=="" = input (xs,y)
     | x=="groupadd"  = groupadd (xs,y)
     | x=="useradd"= useradd (xs,y)
     | x=="show"= showatributes (xs,y)
     | x=="finger" = showuseraccount (xs,y)
     | x=="userdel" = userdel (xs,y)
     | x=="groupdel" = groupdel (xs,y)

     | otherwise = "Couldn't  find: " ++ x

--Check that exist the command -g
usermod::  ([[Char]],String) -> String
usermod ([],y)= " Couldn't modify user,  instruction incomplete,expected : [-g primaryGroup] [-G secondaryGroup1, secondaryGroup2] userName"
usermod (x:xs,y)
     | x=="" = usermod (xs,y) --if the element is empty or a space call recursively the same funtion
     | x=="-g" = useraddgroup (xs,y)

     | otherwise =  y ++ " : Couldn't create user, expected -g  " 

--Remove  users
userdel:: ([[Char]],String) -> String
userdel ([],y) = "Couldn't remove the user , instruction incomplete,expected: userName"
userdel (x:xs,y)
     | x=="" = userdel (xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = " Couldn't remove the user with: " ++ y -- If after the command are more information o text, 
     --the syntax is incorrect
     | finduser x =  "Removing  userName: " ++ x  -- If the user exist,  it remove the user
     | otherwise =  " Couldn't   find the userName: " ++ x -- If the user doesn't exist

--Remove  groups
groupdel:: ([[Char]],String) -> String
groupdel ([],y) = "Couldn't remove the  group , instruction incomplete,: expected groupName"
groupdel (x:xs,y)
     | x=="" = userdel (xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = " Couldn't remove the group with: " ++ y -- If after the command are more information o text, 
     --the syntax is incorrect
     | findgroup x =  "Removing  groupName: " ++ x  -- If the group exist, it remove the group
     |  otherwise =  " Couldn't   find the groupName: " ++ x -- If the group doesn't exist


--Check what list show, users or groups
showatributes:: ([[Char]],String) -> String
showatributes ([],y) = "Couldn't show, instruction incomplete,expected: users or groups"
showatributes (x:xs,y) 
     | x=="" = showatributes (xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = " Couldn't show information with: " ++ y -- If after the command are more information o text, 
     --the syntax is incorrect
     | x=="users" = showusers
     | x=="groups" = showgroups

     | otherwise = " Couldn't find:  " ++ x --If the group exist doesn't add the new group

--The following  funtion will be used to  show all the existent user groups in the
--system with their corresponding attributes
showgroups:: String
showgroups = "Showing Groups"

--The following  funtion will be used to  show all the existent user users in the
--system with their corresponding attributes
showusers:: String
showusers = "Showing Users"

--The following  funtion will be used to  show the details of a specific user account
showuseraccount:: ([[Char]],String) ->String
showuseraccount ([],y) = " Couldn't show the user account, instruction incomplete, expected: name"
showuseraccount (x:xs,y)
     | x=="" = showuseraccount (xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = " Couldn't  show the user account with: " ++ y -- If after the name are more information o text, 
     --the syntax is incorrect
     | finduser x =  "Showing User Account" -- If the user exist, show its atributes
     |  otherwise =  " Couldn't   find the user account: " ++ x -- If the user doesn't exist


--Add a new group
groupadd::  ([[Char]],String) -> String
groupadd ([],y) = " Couldn't create group, expected: groupName"
groupadd (x:xs,y)
     | x=="" = groupadd (xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = " Couldn't create group with: " ++ y -- If after the name are more information o text, 
     --the syntax is incorrect
     | findgroup (x) = " Couldn't add, existing group  " --If the group exist doesn't add the new group
     | otherwise =  "  group added: " ++ "  groupName: "  ++ x--If the group doesn't exist add the  new group

--Check that exist the command -g
useradd::  ([[Char]],String) -> String
useradd ([],y)= " Couldn't create user, instruction incomplete, expected : -g primaryGroup userName"
useradd (x:xs,y)
     | x=="" = useradd (xs,y) --if the element is empty or a space call recursively the same funtion
     | x=="-g" = useraddgroup (xs,y)

     | otherwise =  y ++ " : Couldn't create user, expected -g  " 
   
    
--Check the name of primary Group
useraddgroup::  ([[Char]],String)-> String
useraddgroup ([],y) =  " Couldn't create user, instruction incomplete, expected: name primary group and UserName"
useraddgroup (x:xs,y)
     | x=="" = useraddgroup (xs,y)--if the element is empty or a space call recursively the same funtion
     

     | findgroup (x) =  useradd_secondgroup (xs,y)--If the group exist so, add the group and call the funtion 
     --useradd_secondgroup to check the next command
     | otherwise =  "Couldn't find: " ++ x--If the group doesn't exist so can't add the user



--Check if the user have a second group or not
useradd_secondgroup::  ([[Char]],String)-> String
useradd_secondgroup ([],y)= " Couldn't create user, instruction incomplete, expected:   UserName"
useradd_secondgroup (x:xs,y)

     | x=="" = useradd_secondgroup (xs,y) --if the element is empty or a space call recursively the same funtion
     | (x=="-G")  && null xs = " Couldn't create user, expected   SeconfGroupName"--If the command was -G and the 
     --list is empty, the instruction is incomplete
     | (x=="-G")  && (length xs<=1) = " Couldn't create user, expected   UserName"--If the command was -G and is 
     --only the SecondGroupName, the instruction is incomplete
     | null xs = useradd_name x--If are one element in the list, that element is the UserName
 
     | x=="-G" = useradd_secondgroup (xs,y)--If the command  is -G and there are elements in the list, call recursively 
     --the same funtion
     | findgroup (x)=  useradd_secondgroup (xs,y)--If the Group exist, add the second group and call recursively the same 
     --funtion

     | otherwise = "Couldn't find: " ++ x--If the group doesn't exist , can't add the user

--Add the new User
useradd_name::  String -> String
useradd_name x
   

     | finduser x =  " Couldn't add, existing User"-- If the user exist, doesn't add the new user
     |  otherwise =  "User added: " ++ "UserName: " ++ x -- If the user doesn't exist, add the ner user

--Check if the user exist
finduser::  String -> Bool
finduser x
     | null x = False --If is a empty name, return false
     | x=="" = False --If is a space, return false
     
 
     | otherwise =  True --Return True when find the user ( in this instruction,lack the users list )

---Check if the group exist
findgroup::  String -> Bool
findgroup x
     | null x = False --If is a empty name, return false
     | x=="" = False --If is a space, return false
     
 
     | otherwise =  True --Return True when find the user ( in this instruction,lack the groups list)