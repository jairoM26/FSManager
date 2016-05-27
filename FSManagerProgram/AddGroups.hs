{-# OPTIONS_GHC -fno-warn-tabs #-}
module AddGroups
(groupadd
, findgroup
)
where 
import GetData

--Add a new group
groupadd:: ([(String,Int,[String])],[(String,Int,String,[String])],[[Char]],String)-> ([(String,Int,[String])], [(String,Int,String,[String])],String)
groupadd (pGroupList,pUserList,[],y) = (pGroupList,pUserList," Couldn't create group, expected: groupName")
groupadd (pGroupList,pUserList,x:xs,y)
     
     | (length xs>=1) = (pGroupList,pUserList," Couldn't create group with: " ++ y) -- If after the name are more information o text, 
     --the syntax is incorrect
     | findgroup (pGroupList,x) = (pGroupList,pUserList," Couldn't add, existing group  " )--If the group exist doesn't add the new group
     | otherwise = if (length pGroupList) == 0
                    then (pGroupList ++ [(x, 1000, [])],pUserList,"")
                    else do (pGroupList ++ [(x, (tupleGetSecond' (last pGroupList)) + 1, [])],pUserList,"")--If the group doesn't exist add the  new group

---Check if the group exist
findgroup:: ([(String,Int,[String])],String) -> Bool
findgroup (pGroupList, x)
     | null pGroupList = False
     | (x `elem` (generateNamesGroupList pGroupList)) =  True
     | otherwise = False                   