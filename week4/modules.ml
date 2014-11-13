
module type DICT = sig
    (* t is abstract *)
    type ('a, 'b) t
    val empty : ('a,'b) t
    val put : 'a -> 'b -> ('a,'b) t -> ('a,'b) t
    val get : 'a -> ('a,'b) t -> 'b option
  end


module Dict1 = struct

  type ('a,'b) t = ('a * 'b) list
  
  let empty = []
      
  let put k v d = (k,v)::d

  let rec get k d =
    match d with
  	[] -> None
      | (k',v')::d' -> if k=k' then Some v' else get k d'
end


module Dict2:DICT = struct
     type ('a,'b) t = Empty | Entry of 'a * 'b * ('a,'b) t
    
  let empty = Empty
  
  let put k v d = Entry (k,v,d)
    
  let rec get k d =
    match d with
	Empty -> None
      | Entry (k', v', d') ->
	  if k'=k then Some v' else (get k d')
end
   
(* we'd like to separate *interface* from *implementation*

   above we're using lists as our implementation of dictionaries
   and this is exposed to clients of the module.
   why is this undesirable?
   - maintainability / software evolution
     - the implementer wants flexiblity to change the implementation
       without breaking clients
   - security/privacy - want to be able to hide some data
   - might be necessary for correctness
     - example: consider a set implemented as a list
     - can't ensure the invariant that the list has no duplicates

  solution: use an explicit module type (aka "signature") to hide
   the implementation type
*)
