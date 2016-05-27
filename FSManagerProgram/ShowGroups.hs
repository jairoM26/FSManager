{-# OPTIONS_GHC -fno-warn-tabs #-}
module ShowGroups 
(showgroups
)
where 
import GetData

--The following  funtion will be used to  show all the existent user groups in the
--system with their corresponding attributes
showgroups:: ([(String,Int,[String])],[(String,Int,String,[String])])->([(String,Int,[String])],[(String,Int,String,[String])],String)
showgroups (pGroupList,pUserList) = showgroupsAux (pGroupList,pGroupList,pUserList,"GroupName                          GID           AssociatedUsers " ++ "\n")

showgroupsAux :: ([(String,Int,[String])],[(String,Int,[String])],[(String,Int,String,[String])],String)->([(String,Int,[String])],[(String,Int,String,[String])],String)
showgroupsAux  ([],pList,pUserList,pMessage) = (pList,pUserList,pMessage)
showgroupsAux (x:pGroupList,pList, pUserList, pMessage )= showgroupsAux (pGroupList,pList, pUserList, pMessage ++ tupleGetFirst' x ++ addspaces( 36- length(tupleGetFirst' x),"") ++ show(tupleGetSecond' x) ++ addspaces( 10,"") ++ generateAssocUserGroupString (tupleGetThird' x) ++  "\n")
