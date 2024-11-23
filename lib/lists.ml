let rec has_prefix lst pre = 
  match lst, pre with 
  | _, [] -> true
  | [], _ -> false 
  | h1::tl1, h2::tl2 -> h1 = h2 && has_prefix tl1 tl2

let rec contains_slice lst slc = 
  match lst, slc with 
  | [], [] -> true 
  | [], _ -> false 
  | _::tl, _  -> has_prefix lst slc || contains_slice tl slc 

