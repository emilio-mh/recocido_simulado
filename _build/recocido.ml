
let phi      = ref 0.8      in
let temp_ini = ref 100.0    in
let temp_min = ref 1.0      in
let semilla  = ref 23       in
let lote_tam = ref 100      in

Arg.parse[
  ("-phi", Arg.Float (function i -> phi      := i), "Factor de enfriamiento");
  ("-ti" , Arg.Float (function i -> temp_ini := i), "Temperatura inicial"   );
  ("-tm" , Arg.Float (function i -> temp_min := i), "Temperatura minima"    );
  ("-s" ,  Arg.Int   (function i -> semilla  := i), "Semilla para el PRG"   );
  ("-l" ,  Arg.Int   (function i -> lote_tam := i), "Tamaño del lote"       );
  ](function s -> ()) "Sin parametros especificados" ;

let () = Random.init !semilla in

let file = open_in "problema" in
let line = input_line file in

let inst = Array.of_list (List.map (function x -> int_of_string x) (String.split_on_char ',' line)) in
let n    = Array.length inst in


let rand = (fun _ -> (fun _ -> (Random.int n) - 1)) in



Array.sort rand inst;


let vecino x = 
  (let i = Random.int 3 in
  let j = Random.int 3 in
  let aux = Array.get inst i in
  Array.set inst i (Array.get inst j);
  Array.set inst j aux;
  x) in 


let costo = (fun _ -> 1.0) in


let calculalote (t : float) (s : int array ref)  =
  let fs = costo !s in
  let c = ref 0   in
  let r = ref 0.0   in
  let ss = ref (vecino !s) in
  while !c < !lote_tam do
    ss := vecino !s;
    let fss = costo ss in
    if fss < fs +. t then
      c := !c + 1;
      s := !ss;
      r := !r +. fss;
      ()
  done;
  (!r /. float_of_int (!lote_tam), s) in

rand 1

