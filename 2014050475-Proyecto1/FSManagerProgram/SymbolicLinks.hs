{-# OPTIONS_GHC -fno-warn-tabs #-}
module SymbolicLinks 
(
)
where 
import GetData

ln :: ([(String,Int,[String])],[(String,Int,String,[String])],[(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)],[(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)],[[Char]],String)->([(String,Int,[String])],[(String,Int,String,[String])],([(String,Int,String,[Bool])],[(String,[String],[String],Int,Int)],[(String,Int,Bool)]),([(String,String,Char,[String],Int,String)],[(String,String,[String],[String],String,String,String,String,String,Bool)],[(String,String,String,String,String,String)]),String)
ln (pGroupList,pUserList,pStorageList,pVGList,pLVList,pFileSystList,directoriesList,filesList,[],y) = (pGroupList,pUserList,(pStorageList,pVGList,pLVList),(pFileSystList,directoriesList,filesList), "Couldn't do ln, expected: ln -s <sourceFile/Dir> <targetPath>")