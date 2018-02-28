open Printf;;
open Sqlite3;;


let semilla = ref 7          in
let lote    = ref 25         in
let temp_in = ref 1.1        in
let temp_m  = ref 0.001      in
let phi     = ref 0.99       in
let source  = ref "problema" in
let evals   = ref 1          in
begin

Arg.parse[
  ("-phi" , Arg.Float  (function i -> phi     := i), "Factor de enfriamiento"                 );
  ("-ti"  , Arg.Float  (function i -> temp_in := i), "Temperatura inicial"                    );
  ("-tm"  , Arg.Float  (function i -> temp_m  := i), "Temperatura minima"                     );
  ("-s"   , Arg.Int    (function i -> semilla := i), "Semilla para el PRG"                    );
  ("-l"   , Arg.Int    (function i -> lote    := i), "Tamaño del lote"                        );
  ("-src" , Arg.String (function i -> source  := i), "Archivo con la descripción del problema");
  ("-n"   , Arg.Int    (function i -> evals   := i), "Númro de evaluaciones"                  );
  ](function s -> ()) "Error leyendo parámetros" ;


  let temp_i = ref !temp_in in

Printf.printf "phi = %f\n" !phi;
Printf.printf "t_i = %f\n" !temp_i;
Printf.printf "t_m = %f\n" !temp_m;
Printf.printf "sem = %d\n" !semilla;
Printf.printf "lot = %d\n" !lote;

Random.init !semilla;

let imprimir_arreglo out a = 
for i = 0 to (Array.length a) - 1 do
  fprintf out "%d " (a).(i)
done;
in



let file = open_in !source in
let lin = input_line file in
close_in file;


let re =  Str.regexp "[\r\n]" in
let line = Str.global_replace re "" lin in

let instancia = Array.of_list (List.map int_of_string (String.split_on_char ',' line)) in


(*Inicio de la lectura de la base de datos*)


  let db = db_open "tsp.db" in
  (*Leer el tamaño de la base de datos*)

  let e = ref 0 in
  let strr = (Array.fold_left (^) "" (Array.map ((^) ",") (Array.map string_of_int instancia)))in
  let str = String.sub strr 1 ((String.length strr) - 1) in

  exec db "SELECT count(id) FROM cities" ~cb:(fun row _ ->
    match row.(0) with
      | Some a -> e := int_of_string a
      | _ -> ());

  (*Printf.printf  "%s\n" str;*)

  let costos = Array.make_matrix (!e + 1) (!e + 1) 12982205.69 in
  let sum = ref 0.0 in
  let e_s = ref 0   in

  (*Leer las distancias que se usarán*)
  begin
    printf "%s\n" str;
    exec db (sprintf "SELECT id_city_1, id_city_2, distance FROM connections WHERE id_city_1 IN (%s) AND id_city_2 IN (%s)" str str)
      ~cb:(fun row _ ->
      match row.(0), row.(1), row.(2) with
        | Some a, Some b, Some c ->
          let c1 = int_of_string   a in
          let c2 = int_of_string   b in
          let w  = float_of_string c in
          Array.set costos.(c1) c2 w;
          Array.set costos.(c2) c1 w;
          sum := !sum +. w;
          e_s := !e_s + 1
        | _ -> ());
  end;


  (*Fin de la lectura de la base de datos*)

  let promedio = !sum /. (float_of_int (!e_s )) in
  let n = Array.length instancia in

  let costo (s : int array) : float = 
    let w = ref 0.0 in
    for i = 0 to (Array.length s) - 2 do
      w := !w +. (costos.(s.(i)).(s.(i+1)))
    done;
    !w /. (promedio *. (float_of_int (n - 1)) ) in

  printf "%2.9f\n" (costo instancia);


let mejor_solucion = ref instancia in
let mejor_costo = ref (costo instancia) in

let rec barrido (s : int array) : (int array) = 
  let nv = ref (Array.copy s) in
  for i = 0 to (Array.length s) -1 do
    for j = i + 1 to (Array.length s) -1 do
      Array.set !nv i (Array.get s j);
      Array.set !nv j (Array.get s i);
      if costo !nv < costo s then (
        nv := barrido !nv
      ) else (
        Array.set !nv j (Array.get s j);
        Array.set !nv i (Array.get s i);
      )

    done
  done;
  !nv in



let vecino (s : int array ref) : (int array ref) =
  let i = Random.int n in
  let j = Random.int n in
  let r = Array.copy !s in
  Array.set r i (Array.get !s j);
  Array.set r j (Array.get !s i);
  ref r in

let calcula_lote (t : float) (s : int array ref) : float*(int array) =
  let c = ref 0   in
  let r = ref 0.0 in
  let i = ref 0   in
  while !c < !lote && !i < !lote * !lote do(
    let ss = vecino s in 
    i := !i + 1;
    let fs = costo !ss in
    if fs < (costo !s) +. t then (   
        if fs < !mejor_costo then (
          printf "E: %2.9f\n" fs;
          mejor_solucion := Array.copy !ss;
          mejor_costo := fs;
          
        );
        s := !ss;
        r := !r +. fs;
        c := !c + 1;
    );
  )done;
  ((!r /. (float_of_int !c)), !s) in

let umbrales (t : float ref) (s : int array ref) : unit =
  Array.sort (fun _ _ -> (Random.int 3) - 1) instancia;
  printf "Inicio semilla %d\n" !semilla;
  let p = ref 0.0 in
  let i = ref 0 in
  while !t > !temp_m do (
    i := !i + 1;
    let q = ref Pervasives.max_float in
    while !p < !q do(
      q := !p;
      let (r, ns) = calcula_lote !t s in
      p := r;
      s := ns;
    )done;
    t := !t *. !phi
  )done;
  in

let factible (s : int array) = 
  let fact = ref true in
  for i = 0 to (Array.length !mejor_solucion) - 2 do
    let c1 = (Array.get !mejor_solucion i) in
    let c2 = (Array.get !mejor_solucion (i + 1)) in
    let sql = (sprintf "select count(distance) from connections where (id_city_1 = %d and id_city_2 = %d) or (id_city_1 = %d and id_city_2 = %d)" c1 c2 c2 c1) in 
    match exec db sql ~cb:(fun r _ ->
      match r.(0) with
        | Some a ->  if 0 = (int_of_string a) then fact := false
        | _ -> () ) with
    | Rc.OK -> ()
     | _ -> ()
  done; !fact in


let rec eval (m : int) =
  if m = 0 then
    ()
  else(
    temp_i := !temp_in;
    semilla := !semilla + n;
    Random.init !semilla;
    umbrales temp_i (ref instancia);
    let out = open_out (sprintf "reportes/emilio%d-%n.tsp" n !semilla) in
    imprimir_arreglo out !mejor_solucion;
    fprintf out "\n%2.9f\n" !mejor_costo;
    (*fprintf out "Fctible %B" (factible !mejor_solucion);*)
    close_out out;
    flush stdout;
    eval (m-1)
  ) in

let () = eval !evals   in  


db_close db;
();
end;()