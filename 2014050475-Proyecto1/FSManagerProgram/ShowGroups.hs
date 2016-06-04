{-# OPTIONS_GHC -fno-warn-tabs #-}
module ShowGroups 
(showgroups
)
where 
import GetData

--The following  funtion will be used to  show all the existent user groups in the
--system with their corresponding attributes
showgroups:: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)])->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
showgroups (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList) = showgroupsAux (pGroupList,pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,"GroupName                          GID           AssociatedUsers " ++ "\n")

showgroupsAux :: ([(String,Int,[String])],[(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
showgroupsAux  ([],pList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage) = (pList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),pMessage)
showgroupsAux (x:pGroupList,pList, pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList, pMessage )= showgroupsAux (pGroupList,pList, pUserList, storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage ++ tupleGetFirst' x ++ addspaces( 36- length(tupleGetFirst' x),"") ++ show(tupleGetSecond' x) ++ addspaces( 10,"") ++ generateAssocUserGroupString (tupleGetThird' x) ++  "\n")
