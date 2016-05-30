{-# OPTIONS_GHC -fno-warn-tabs #-}
module ShowVolumeGroups 
(vgdisplay
)
where
import GetData
import Data.Tuple.Select

{-
-}
vgdisplay :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
vgdisplay (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,[],y) =  simpleDisplay (pGroupList,pUserList,storageList,pVolumeGroupsList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,"---- volume Group ----" ++ "\n")
vgdisplay (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,x:xs,y)
	| x == "" = vgdisplay (pGroupList,pUserList,storageList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,xs,y)
	| x == "-v" && (length xs) == 0 = detailDisplay (pGroupList,pUserList,storageList,pVolumeGroupsList,pVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,"---- volume Group ----" ++ "\n")
	| x == "-v" && (length xs) == 1 = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), "---- volume Group ----" ++ "\n" ++ getSimpleMessage (getVG (pVolumeGroupsList,head xs)) ++ getLVMessage(pLVolume,sel3 (getVG (pVolumeGroupsList,head xs))) ++ getPVMessage(storageList,sel2 (getVG (pVolumeGroupsList,head xs))))
	| otherwise = (pGroupList,pUserList,(storageList,pVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"Couldn't show the volume groups, couldn't find: "++ x)

{-
-}
simpleDisplay :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int)],[(String,String,[String],[String],String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
simpleDisplay (pGroupList,pUserList,storageList,[],tmpVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage) = (pGroupList,pUserList,(storageList,tmpVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),"")
simpleDisplay (pGroupList,pUserList,storageList,vgTmp:pVolumeGroupsList,tmpVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage)
	| null pVolumeGroupsList = (pGroupList, pUserList,(storageList,tmpVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),pMessage ++ getSimpleMessage (vgTmp))
	| otherwise = simpleDisplay (pGroupList, pUserList,storageList,pVolumeGroupsList,tmpVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage ++ getSimpleMessage (vgTmp) ++ "\n"++"---- volume Group ----" ++ "\n")  

{-
-}
getSimpleMessage :: (String,[String],[String],Int,Int)->String
getSimpleMessage vg = "VG Name" ++ addspaces(20-7,"")++(sel1 vg)++"\n" ++ "Cur LV" ++ addspaces(20-6," ")++show (length (sel3 vg)) ++
					"\n" ++ "Cur PV" ++ addspaces(20-6," ") ++ show (length (sel2 vg))++ "\n" ++ "VG Size" ++ addspaces(20-7," ")++show (sel4 vg) ++ " M"++ "\n" ++
						"Free Size" ++ addspaces(20-9," ") ++ show (sel5 vg) ++ " M" ++ "\n"

{-
-}
detailDisplay (pGroupList,pUserList,storageList,[],tmpVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage) = (pGroupList,pUserList,(storageList,tmpVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList),pMessage)
detailDisplay (pGroupList,pUserList,storageList,vgTmp:pVolumeGroupsList,tmpVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage)
	| null pVolumeGroupsList = (pGroupList,pUserList, (storageList, tmpVolumeGroupsList,pLVolume),(pFileSystList,directoriesList,filesList), pMessage ++ (getSimpleMessage vgTmp) ++ "\n"++"--- Logical Volume---" ++ "\n" ++(getLVMessage (pLVolume,sel3 vgTmp)) ++ "\n" ++"--- Physical Volume---" ++(getPVMessage (storageList,sel2 vgTmp)))
	| otherwise = detailDisplay (pGroupList,pUserList,storageList,pVolumeGroupsList,tmpVolumeGroupsList,pLVolume,pFileSystList,directoriesList,filesList,pMessage ++ (getSimpleMessage vgTmp) ++ "\n"++"--- Logical Volume---" ++ "\n" ++(getLVMessage (pLVolume,sel3 vgTmp))++ "\n" ++ "--- Physical Volume---" ++ "\n" ++(getPVMessage (storageList,sel2 vgTmp)) ++ "\n"++"---- volume Group ----" ++ "\n")

{-
-}
getPVMessage (storageList,[]) = ""
getPVMessage (storageList,tmp:pvList) = "\n" ++ "PV Name" ++ addspaces(20-7," ") ++ tmp ++ "\n" ++ "Total Size" ++ addspaces(20-10," ") ++ show(sel2 (getDevice (storageList,tmp))) ++ "M"

{-
-}
getLVMessage (logicalVList,[]) = ""
getLVMessage (logicalVList,tmp:lvList)= "\n" ++ "LV Name" ++ addspaces(20-7," ") ++ tmp ++ "\n" ++ "Total Size" ++ addspaces(20-10," ") ++ show(sel2 (getLV (logicalVList,tmp))) ++ "M"

