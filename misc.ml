open Printf;;

open Sqlite3;;

let imprimir_arreglo out a = 
for i = 0 to (Array.length a) - 1 do
  fprintf out "%d " (a).(i)
done

let factible (s : int array) = 
  let fact = ref true in
  let db = db_open "tsp.db" in 
  for i = 0 to (Array.length s) - 2 do
    let c1 = (Array.get s i) in
    let c2 = (Array.get s (i + 1)) in
    let sql = (sprintf "select count(distance) from connections where (id_city_1 = %d and id_city_2 = %d) or (id_city_1 = %d and id_city_2 = %d)" c1 c2 c2 c1) in 
    match exec db sql ~cb:(fun r _ ->
      match r.(0) with
        | Some a ->  if 0 = (int_of_string a) then fact := false
        | _ -> () ) with
    | Rc.OK -> ()
    | _ -> ();
  done;
  db_close db;
  !fact