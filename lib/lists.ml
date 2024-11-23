(** Determine if a given list [lst] has a prefix [pre] -> bool **)
let rec has_prefix lst pre = 
  match lst, pre with 
  | _, [] -> true
  | [], _ -> false 
  | h1::tl1, h2::tl2 -> h1 = h2 && has_prefix tl1 tl2

  (** Determine if a given list [lst] contains a sublist, or slice, 
    [slc] in it as opposed to containing the elements of the sublist
    or slice. 
     -- Note: the elements in the slice [slc] must be in ascending
    order
  **)
let rec contains_slice lst slc = 
  match lst, slc with 
  | [], [] -> true 
  | [], _ -> false 
  | _::tl, _  -> has_prefix lst slc || contains_slice tl slc 

  (** Determine if a given list [lst] has the elements of a sublist, or 
    slice [slc] in it; regardless of whether they occur sequentially
    or not
    -- Note: the elements in the slice [slc] must be in ascending
    order
  **)
let rec contains_els lst slc = 
  match lst, slc with
  | _, [] -> true
  | [], _ -> false 
  | h1::t1, h2::t2 when h1 = h2 -> contains_els t1 t2 
  | _::t1, _ -> contains_els t1 slc 

