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
let sum = ref 0.0 in
let count = ref 0 in
for i = 1 to !e do
  for j = (i + 1) to !e do
    let sql = sprintf "SELECT distance FROM connections WHERE id_city_1 = %d AND id_city_2 = %d" i j in
    exec db sql ~cb:(fun row _ ->
    match row.(0) with
      | Some d ->
        let w = float_of_string d in
        Array.set costos.(i) j w;
        Array.set costos.(j) i w;
        sum := !sum +. w;
        count := !count + 1
      | _->
        let l1 = ref 0.0 in
        let l2 = ref 0.0 in
        let p1 = ref 0.0 in
        let p2 = ref 0.0 in
        (
        exec db (sprintf "SELECT latitude, longitude FROM cities WHERE id = %d" i) ~cb:(fun row _ ->
        match row.(0), row.(1) with
          | Some lat, Some lon -> p1 := (float_of_string lat); l1 := (float_of_string lon); ()
          | _, _ -> ());
        exec db (sprintf "SELECT latitude, longitude FROM cities WHERE id = %d" j) ~cb:(fun row _ ->
        match row.(0), row.(1) with
          | Some lat, Some lon -> p2 := (float_of_string lat); l2 := (float_of_string lon); ()
          | _, _ -> ()); ());
          let w = 5.5 *. 6335.439 *. acos ( (sin !p1) *. (sin !p2) +. (cos !p1) *. (cos !p2) *. (cos (abs_float (!l1 -. !l2)))) in
          Array.set costos.(i) j w;
          Array.set costos.(j) i w)
  done
done;
let prom = !sum /. (float_of_int !count) in

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

let costo s =
  let w = ref 0.0 in
  for i = 0 to (Array.length s) - 2 do
    let tmp = costos.(Array.get s i).(Array.get s (i + 1)) in
    w := !w +. tmp
  done; !w /. (prom *. (float_of_int !e) -. 1.0) in


let calculalote t s  =
  let fs = costo !s in
  let c = ref 0   in
  let r = ref 0.0   in
  let ss = ref (vecino !s) in
  while !c < !lote_tam do
    ss := vecino !s;
    let fss = costo !ss in
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