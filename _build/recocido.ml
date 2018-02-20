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

let file = open_in "problema" in
let line = input_line file in

let inst = Array.of_list (List.map (function x -> int_of_string x) (String.split_on_char ',' line)) in
let n    = Array.length inst in

let costos = ref (Array.make 1 1) in 
(
  let db = db_open "tsp" in
  4
);


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

let imprimir_sol (s : int array) (f : float) =
  Printf.printf "%f : " f;
  for i = 0 to (Array.length s) - 1 do
    Printf.printf "%d " (Array.get s i)
  done;
  Printf.printf "\n" in


let costo = (fun _ -> 1.0) in


let calculalote (t : float) (s : int array ref)  =
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

let aceptacionumbrales (t : float ref) (s : int array ref) = 
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