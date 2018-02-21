open Printf;;
open Sqlite3;;

let phi      = ref 0.8      in
let temp_ini = ref 100.0    in
let temp_min = ref 1.0      in
let semilla  = ref 23       in
let lote_tam = ref 100      in
begin

Arg.parse[
  ("-phi", Arg.Float (function i -> phi      := i), "Factor de enfriamiento");
  ("-ti" , Arg.Float (function i -> temp_ini := i), "Temperatura inicial"   );
  ("-tm" , Arg.Float (function i -> temp_min := i), "Temperatura minima"    );
  ("-s" ,  Arg.Int   (function i -> semilla  := i), "Semilla para el PRG"   );
  ("-l" ,  Arg.Int   (function i -> lote_tam := i), "Tamaño del lote"       );
  ](function s -> ()) "Sin parametros especificados" ;

Printf.printf "phi = %f\n" !phi;
Printf.printf "t_i = %f\n" !temp_ini;
Printf.printf "t_m = %f\n" !temp_min;
Printf.printf "sem = %d\n" !semilla;
Printf.printf "lot = %d\n" !lote_tam;


let () = Random.init !semilla in

let rec imprimir_lista l = match l with
  | [] -> Printf.printf "\n"
  | e :: t -> Printf.printf "%s " e; imprimir_lista t in
let imprimir_sol s f =
  Printf.printf "%f : " f;
  for i = 0 to (Array.length s) - 1 do
    Printf.printf "%d " (Array.get s i)
  done;
  Printf.printf "\n" in

let file = open_in "problema" in
let line = input_line file in
let siz = String.length line in
let liine = String.sub line 0 (siz-1) in
let tmp = String.split_on_char ',' liine in
let inst = Array.of_list (List.map int_of_string  tmp)in
imprimir_sol inst 1.1 ;
let n    = Array.length inst in


let db = db_open "tsp.db" in
let e = ref 0 in
exec db "SELECT count(id) FROM cities" ~cb:(fun row _ ->
  match row.(0) with
  | Some a -> e := int_of_string a
  | _ -> ());
Printf.printf "%d\n" !e;
let costos = Array.make_matrix (!e + 1) (!e + 1) 0.0 in
let res =
  exec db "SELECT * FROM connections" ~cb:(fun row _ ->
  match row.(0), row.(1), row.(2) with
  | Some a, Some b, Some d ->
  let c1 = int_of_string a in
  let c2 = int_of_string b in
  let w = float_of_string d in
  Array.set costos.(c2) c1 w;
  Array.set costos.(c1) c2 w
  | _, _, _ -> ()) in

let rand = (fun _ -> (fun _ -> (Random.int n) - 1)) in



Array.sort rand inst;


let vecino x = 
  let n = Array.length x in
  (let i = Random.int n in
  let j = Random.int n in
  let aux = Array.get inst i in
  Array.set inst i (Array.get inst j);
  Array.set inst j aux;
  x) in 


let costo = (fun _ -> 1.0) in


let calculalote t s  =
  let fs = costo !s in
  let c = ref 0   in
  let r = ref 0.0   in
  let ss = ref (vecino !s) in
  while !c < !lote_tam do
    ss := vecino !s;
    let fss = costo ss in
    if fss < fs +. t then (*Acepta nueva solución*)
      imprimir_sol !s fss;
      c := !c + 1;
      s := !ss;
      r := !r +. fss;
      ()
  done;
  (!r /. float_of_int (!lote_tam), s) in

let aceptacionumbrales t s = 
  let p = ref 0.0 in
  while t > temp_min do
    let q = ref Pervasives.max_float in
    while !p < !q do
      q := !p;
      let (x, y) = calculalote !t s in
      p := x;
      s := !y;
    done;
    t := !t *. !phi
  done in

aceptacionumbrales temp_ini (ref inst)

end