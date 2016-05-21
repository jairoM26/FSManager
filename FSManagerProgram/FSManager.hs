-- @author: Jairo Méndez 
-- @author: Cristian Castillo
-- @date: 18 - 05 -2016
-- @File: FSManager.hs
-- @brief: A virtual file system manager

--Function main: start the FSManager

-----------------------------------------------------------------------------------------------------------------------------------
									-- GETTERS FOR TOUPLE
-----------------------------------------------------------------------------------------------------------------------------------

--Functions for return first, second and third element of a tuple
--pFirst, pSecond and pThird can be any type as they want
tuple_first' :: (pFirst, pSecond, pThird) -> pFirst  
tuple_first' (x, _, _) = x  
  
tuple_second' :: (pFirst, pSecond, pThird) -> pSecond  
tuple_second' (_, y, _) = y  
  
tuple_third' :: (pFirst, pSecond, pThird) -> pThird 
tuple_third' (_, _, z) = z  

tuple_first :: (pFirst, pSecond, pThird, pFourth) -> pFirst  
tuple_first (x, _, _,_) = x  
  
tuple_second :: (pFirst, pSecond, pThird, pFourth) -> pSecond  
tuple_second (_, y, _,_) = y  
  
tuple_third :: (pFirst, pSecond, pThird, pFourth) -> pThird 
tuple_third (_, _, z,_) = z  

tuple_Fourth :: (pFirst, pSecond, pThird, pFourth) -> pFourth
tuple_Fourth (_, _, _,a) = a  

-----------------------------------------------------------------------------------------------------------------------------------
										-- FUNCTION GETTER BY INDEX IN A LIST --
-----------------------------------------------------------------------------------------------------------------------------------
--Functions for return an element of a list with an index
getListElement pIndex list = list !! pIndex


--group function 
