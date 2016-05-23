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

     | otherwise = "Couldn't  find " ++ x

--Add a new group
groupadd::  ([[Char]],String) -> String
groupadd ([],y) = " Couldn't create group, expected name"
groupadd (x:xs,y)
     | x=="" = groupadd (xs,y)--if the element is empty or a space call recursively the same funtion
     | (length xs>=1) = " Couldn't create group with " ++ y -- If after the name are more information o text, the syntax is incorrect
     | findgroup (x) = " Couldn't add, existing group  " --If the group exist doesn't add the new group
     | otherwise =  "  group added " ++ x--If the group doesn't exist add the  new group

--Check that exit the command -g
useradd::  ([[Char]],String) -> String
useradd ([],y)= " Couldn't create user, expected -g primaryGroup userName"
useradd (x:xs,y)
     | x=="" = useradd (xs,y) --if the element is empty or a space call recursively the same funtion
     | x=="-g" = useraddgroup (xs,y)

     | otherwise =  x ++ " Couldn't create user , expected -g"
   
    
--Check the name of primary Group
useraddgroup::  ([[Char]],String)-> String
useraddgroup ([],y) =  " Couldn't create user, expected name primary group and UserName"
useraddgroup (x:xs,y)
     | x=="" = useraddgroup (xs,y)--if the element is empty or a space call recursively the same funtion
     

     | findgroup (x) =  useradd_secondgroup (xs,y)--If the group exist so, add the group and call the funtion useradd_secondgroup to check the next command
     | otherwise =  "Couldn't find " ++ x--If the group doesn't exist so can't add the user



--Check if the user have a second group or not
useradd_secondgroup::  ([[Char]],String)-> String
useradd_secondgroup ([],y)= " Couldn't create user, expected   UserName"
useradd_secondgroup (x:xs,y)

     | x=="" = useradd_secondgroup (xs,y) --if the element is empty or a space call recursively the same funtion
     | (x=="-G")  && null xs = " Couldn't create user, expected   SeconfGroupName"--If the command was -G and the list is empty, the instruction is incomplete
     | (x=="-G")  && (length xs<=1) = " Couldn't create user, expected   UserName"--If the command was -G and is only the SecondGroupName, the instruction is incomplete
     | null xs = useradd_name x--If are one element in the list, that element is the UserName
 
     | x=="-G" = useradd_secondgroup (xs,y)--If the command  is -G and there are elements in the list, call recursively the same funtion
     | findgroup (x)=  useradd_secondgroup (xs,y)--If the Group exist, add the second group and call recursively the same funtion

     | otherwise = "Couldn't find " ++ x--If the group doesn't exist , can't add the user

--Add the new User
useradd_name::  String -> String
useradd_name x
   

     | finduser x =  " Couldn't add, existing User"
     |  otherwise =  "User added " 

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