{-# OPTIONS_GHC -fno-warn-tabs #-}
module ShowUsers
(showUsers
)
where 
import GetData

--The following  funtion will be used to  show all the existent user groups in the
--system with their corresponding attributes
showUsers:: ([(String,Int,[String])],[(String,Int,String,[String])])->([(String,Int,[String])],[(String,Int,String,[String])],String)
showUsers (pGroupList,pUserList) = showUsersAux (pGroupList,pUserList,pUserList,"UserName                            UID           primaryGroup                        SecondaryGroup                      HomeDirectory" ++ "\n")

showUsersAux :: ([(String,Int,[String])],[(String,Int,String, [String])],[(String,Int,String,[String])],String)->([(String,Int,[String])],[(String,Int,String,[String])],String)
showUsersAux  (pGroupList,[],pUserList,pMessage) = (pGroupList,pUserList,pMessage)
showUsersAux (pGroupList,x:pList, pUserList, pMessage) = showUsersAux (pGroupList,pList, pUserList, pMessage ++ tupleGetFirst x ++ addspaces(36- length(tupleGetFirst x),"") ++ show(tupleGetSecond x)   ++ addspaces( 10,"") ++ (tupleGetThird x) ++ addspaces( 36- length(tupleGetThird x),"") ++ generateSecondaryGroupNames (tupleGetFourth x)++ addspaces( 36- length(generateSecondaryGroupNames (tupleGetFourth x)),"") ++ "/home/" ++ tupleGetFirst x ++ "\n")
