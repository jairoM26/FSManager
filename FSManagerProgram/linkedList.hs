{-
 -- @author: Jairo Méndez
 -- @date: 21 - 05 - 2016
 -- @brief: Implementing a simple linkedlist
-}

{-
  --Instructions of how to use
  --creating a variable list
  let list = ("jairo", 1000,[1,2,3]) `Cons` (("jair02", 1001,[3,4,5]) `Cons` EmptyNode)

  getHead list ------ ("jairo", 1000, [1,2,3])

  getTail list ------ Cons ("jair02",1001,[3,4,5]) EmptyNode

  getLength list ---- 2

  getByIndex 0 ---- ("jairo", 1000, [1,2,3])

  -- without variable
  getHead (("jairo", 1000,[1,2,3]) `Cons` (("jair02", 1001,[3,4,5]) `Cons` EmptyNode)) -------- ("jairo",1000,[1,2,3])
  
-}

--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

{-
  --Structure of a linkedlist 
  --A linkedlist always starts empty
  -- Cons add at head,  Cons constructor here. cons is another word for ':'
-}
data LinkedList a = EmptyNode							--Declare the initial state of the list as null
					| Cons a (LinkedList a)				--Adding an element at the start of the list
					deriving (Show, Read, Eq, Ord)      --Adding haskell interfaces as qualities


{-
  --Insert a new element at the beginning of the list
-}
insertHead :: LinkedList a -> a -> LinkedList a 			--Recieve a list, element to insert and return the new list
insertHead pList elemToInsert = Cons elemToInsert pList     --Add the element at the beginning with Cons

{-
  -- Insert a new element at the end of the list
  -- the last instruction add the beginning of the execution it create the list that will be return
  -- Then insert an element that is returned in the second instruction, which concatenate-add the element that we want
-}
insertTail :: LinkedList a -> a -> LinkedList a 										--Recieve a list, element to insert and return the new list
insertTail (Cons a EmptyNode) elemToInsert = Cons a (Cons elemToInsert EmptyNode) 		--add the last element the list have and then insert the element that is want it to be insert
insertTail pList elemToInsert = insertHead (insertTail(getTail pList) elemToInsert) (getHead pList) --

{-
   -- Get the first element of the list
-}
getHead :: LinkedList a -> a 						--Structure of the function recieve a list and return an element
getHead pList = case pList of						--if its a list 
        Cons a _ -> a 								-- return the first element... a _ take the first element and ignore the rest

{-
  -- Get the last element of the list
-}
getLast :: LinkedList a -> a 				--Recieve a list as a parameter en return an element
getLast (Cons a EmptyNode) = a 				--Before the list is empty returns its last element
getLast pList = getLast (getTail pList)     --Reduce the list passed as parameter in recursion

{-
  -- return all the list except the first element
-}
getTail :: LinkedList a -> LinkedList a 			--Structure of the function recieve a list and return a list
getTail EmptyNode = EmptyNode 						--If the list passed as parameter is empty so it returns empty
getTail pList = case pList of 						--if its a list
	Cons _ a -> a 									-- return the list except the first element.... _ a ignore the first element

{-
  -- Return an element accessing into the list with an index
-}
getByIndex :: Int -> LinkedList a -> a 							--Structure of the function recieve an int (index), a list and return an element
getByIndex 0 pList = getHead pList 								--If the index is 0 returns the head(start-first element) of the list
getByIndex index pList = getByIndex (index -1) (getTail pList)	--Generate a recursion reducing the value of the index to 0,
																--and getting the tail of the list, then it returns the element we wants

{- 
  -- Returns the lenght of the list
  -- Reduce the list and increment with recursion the lenght, when the list is empety returns
-}
getLength :: LinkedList a -> Int 						--Recieve a list and returns an Int
getLength EmptyNode = 0									--If the list is empty return the length
getLength pList = 1 + getLength (getTail pList) 		--Recursion of the function and increment 1 


{-
  --Return the storage list into a basic list in haskell [a0, a1,...,an]
-}
convertIntoList :: LinkedList a -> [a]											--Recieve a storage linkedlist an return the list into []
convertIntoList EmptyNode = [] 													--If the list is empty there is nothing to append
convertIntoList pList = [getHead (pList)] ++ convertIntoList (getTail (pList)) 	--Concatenate lists when the head of the list is take it
																				--and reduce the linkedlist in recursion




