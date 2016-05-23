import Control.Monad  
import Data.List.Split


main = forever $ do  
	
    putStr "# "  
    l <- getLine  
    
    putStrLn (input   (splitOn  " " l))

input::  [[Char]] -> String
input []= ""
input (x:xs)

     | x=="" = input xs
     | x=="groupadd"  = groupadd (xs)
     | x=="useradd"= useradd (xs)

     | otherwise =  x ++ "input "

groupadd::  [[Char]] -> String
groupadd [] = " Couldn't create group, expected name"
groupadd (x:xs)
     | x=="" = groupadd xs
     | otherwise =  x  ++ "input  group "

useradd::  [[Char]] -> String
useradd []= " Couldn't create user, expected details"
useradd (x:xs)
     | x=="" = useradd xs
     | x=="-g" = useraddgroup xs

     | otherwise =  x ++ " Couldn't create user , expected -g"
   
    

useraddgroup::  [[Char]] -> String
useraddgroup[] =  " Couldn't create user, expected name primary group"
useraddgroup (x:xs)
     | x=="" = useraddgroup xs
     

     | useradd_findgroup (x) =  useradd_secondgroup xs
     | otherwise =  "Couldn't find " ++ x




useradd_secondgroup::  [[Char]] -> String
useradd_secondgroup[]= " Couldn't create user, expected details"
useradd_secondgroup (x:xs)

     | x=="" = useradd_secondgroup xs
     | x=="-G" = useradd_secondgroup xs
     | (length xs)<=1 =" Couldn't create user, expected name user"
     | (useradd_findgroup (x))==False =  "Couldn't find " ++ x
     | ((useradd_findgroup (x)) && ((length xs)>=2) )=  useradd_secondgroup xs

     | otherwise =  useradd_name xs

useradd_name::  [[Char]] -> String
useradd_name[] = " Couldn't create user, expected name user"
useradd_name (x:xs)

     | x=="" = useradd_name xs
   

     | otherwise =  " User added"

useradd_findgroup::  String -> Bool
useradd_findgroup x
     | null x = False
     | x=="" = False
     
 
     | otherwise =  True