
open Printf;;
open Sqlite3;;


let semilla = ref 524287            in
let lote    = ref 2000              in
let temp_in = ref 1.2               in
let temp_m  = ref 0.00005           in
let phi     = ref 0.99              in
let source  = ref "problema150.tsp" in
let evals   = ref 1                 in
let b       = ref 1                 in
let v       = ref false             in
begin

Arg.parse[
  ("-phi" , Arg.Float  (function i -> phi     := i), "Factor de enfriamiento"                  );
  ("-ti"  , Arg.Float  (function i -> temp_in := i), "Temperatura inicial"                     );
  ("-tm"  , Arg.Float  (function i -> temp_m  := i), "Temperatura minima"                      );
  ("-s"   , Arg.Int    (function i -> semilla := i), "Semilla para el PRG"                     );
  ("-l"   , Arg.Int    (function i -> lote    := i), "Tamaño del lote"                         );
  ("-src" , Arg.String (function i -> source  := i), "Archivo con la descripción del problema" );
  ("-n"   , Arg.Int    (function i -> evals   := i), "Número de evaluaciones"                  );
  ("-b"   , Arg.Int    (function i -> b       := i), "Activa barrido"                          );
  ("-v"   , Arg.Int    (function _ -> v    := true), "Modo verboso"                            );
  ](function s -> ()) "Error leyendo parámetros" ;


let temp_i = ref !temp_in in


(*Lectura de la instancia*)

let file = open_in !source in
let lin = input_line file in
close_in file;

let re =  Str.regexp "[\r\n]" in
let line = Str.global_replace re "" lin in

let instancia = Array.of_list (List.map int_of_string (String.split_on_char ',' line)) in



(*Inicio de la lectura de la base de datos*)

let db = db_open "tsp.db" in

let e = ref 0 in
let strr = (Array.fold_left (^) "" (Array.map ((^) ",") (Array.map string_of_int instancia)))in
let str = String.sub strr 1 ((String.length strr) - 1) in

exec db "SELECT count(id) FROM cities" ~cb:(fun row _ ->
  match row.(0) with
    | Some a -> e := int_of_string a
    | _ -> ());

  let costos = Array.make_matrix (!e + 1) (!e + 1) 12982205.69 in
  let sum = ref 0.0 in
  let e_s = ref 0   in

  (*Leer las distancias que se usarán*)
  begin
    
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

db_close db;

  (*Fin de la lectura de la base de datos*)

let promedio = !sum /. (float_of_int (!e_s )) in

let costo = Evaluacion.costo costos promedio in

let n = Array.length instancia in


let mejor_solucion = ref instancia in
let mejor_costo = ref (costo instancia) in
let mejoro = ref false in

let barrido = Barrido.barrido in
let barrido3 = Barrido.barrido3 in
let imprimir_arreglo = Misc.imprimir_arreglo in

let vecino (s : int array ref) : (int array ref) =
  let i = Random.int n in
  let j = Random.int n in
  let r = Array.copy !s in
  Array.set r i (Array.get !s j);
  Array.set r j (Array.get !s i);
  ref r in

let ai = ref 0 in

let calcula_lote (t : float) (s : int array ref) : float*(int array) =
  let c = ref 0   in
  let r = ref 0.0 in
  let i = ref 0   in
  while !c < !lote && !i < !lote * !lote do(
    let ss = vecino s in 
    i := !i + 1;
    let fs = costo !ss in
    if fs < (costo !s) +. t then (   
      if !v then (printf "%d, %2.9f\n" !ai fs;);
      ai := !ai + 1;
      if fs < !mejor_costo then (
        mejoro := true;
        mejor_solucion := Array.copy !ss;
        mejor_costo := fs;
        
        if !b = 1 then (
          mejor_solucion := barrido mejor_solucion costo;
        );
        if !b = 2 then (
          ss := barrido ss costo;
          mejor_solucion := Array.copy !ss;
        );

        let cms = costo !mejor_solucion in
        if !v then ( printf "%d, %2.9f\n" !ai cms;);
        ai := !ai + 1;
        mejor_costo := cms;
      );
      s := !ss;
      r := !r +. fs;
      c := !c + 1;
    );
  )done;
  ((!r /. (float_of_int !c)), !s) in

let umbrales (t : float ref) (s : int array ref) : unit =
  Array.sort (fun x y -> if x > y then 1 else -1) instancia; 
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
  s := barrido3 s costo;
  let cf = costo !s in
  if cf < !mejor_costo then (mejoro := true;);
  in


let rec eval (m : int) =
  if m = 0 then
    ()
  else(
    mejoro := false;
    temp_i := !temp_in;
    semilla := !semilla + m;
    Random.init !semilla;
    umbrales temp_i (ref instancia);
    
    if !mejoro then (
      let out = open_out (sprintf "reportes/emilio%d-%n.tsp" n !semilla) in
      imprimir_arreglo out !mejor_solucion;
      fprintf out "\n%2.9f\n" (costo !mejor_solucion);
      fprintf stdout "\n%2.9f\n" (costo !mejor_solucion);
      close_out out;
      
      flush stdout;
    );
    
    eval (m-1)
  ) in

flush stdout;
let () = eval !evals   in  

();
end