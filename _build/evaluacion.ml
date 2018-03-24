
let costo (m : float array array) (a : float) (s : int array) : float = 
  let w = ref 0.0 in
  for i = 0 to (Array.length s) - 2 do
    w := !w +. (m.(s.(i)).(s.(i+1)))
  done;
  !w /. (a *. (float_of_int (Array.length s - 1)) )

