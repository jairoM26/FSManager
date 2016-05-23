{-
 -- @author: Jairo Méndez
 -- @date: 20 - 05 - 2016
 -- @brief: Implementing functions that will getdata from tuples and lists
-}
-----------------------------------------------------------------------------------------------------------------------------------
									-- GETTERS FOR TOUPLE --
-----------------------------------------------------------------------------------------------------------------------------------
module GetData where 
--Functions for return first, second and third element of a tuple
--pFirst, pSecond and pThird can be any type as they want

{-
 --The functions that have three parameters are used to get the data of the groups
-}
tupleGetFirst' :: (pFirst, pSecond, pThird) -> pFirst  
tupleGetFirst' (x, _, _) = x  
  
tupleGetSecond' :: (pFirst, pSecond, pThird) -> pSecond  
tupleGetSecond' (_, y, _) = y  
  
tupleGetThird' :: (pFirst, pSecond, pThird) -> pThird 
tupleGetThird' (_, _, z) = z  

{-
 --The functions that have four parameters are used to get the data of the groups
-}
tupleGetFirst :: (pFirst, pSecond, pThird, pFourth) -> pFirst  
tupleGetFirst (x, _, _,_) = x  
  
tupleGetSecond :: (pFirst, pSecond, pThird, pFourth) -> pSecond  
tupleGetSecond (_, y, _,_) = y  
  
tupleGetThird :: (pFirst, pSecond, pThird, pFourth) -> pThird 
tupleGetThird (_, _, z,_) = z  

tupleGetFourth :: (pFirst, pSecond, pThird, pFourth) -> pFourth
tupleGetFourth (_, _, _,a) = a  

-----------------------------------------------------------------------------------------------------------------------------------
										-- FUNCTION GETTER BY INDEX IN A LIST --
-----------------------------------------------------------------------------------------------------------------------------------
--Functions for return an element of a list with an index
getListElement pIndex list = list !! pIndex