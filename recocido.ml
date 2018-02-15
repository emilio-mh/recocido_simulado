
let phi      = ref 0.8      in
let temp_ini = ref 100.0    in
let temp_min = ref 1.0      in
let semilla  = ref 23       in

Arg.parse[
  ("-phi", Arg.Float (function i -> phi      := i), "Factor de enfriamiento");
  ("-ti" , Arg.Float (function i -> temp_ini := i), "Temperatura inicial"   );
  ("-tm" , Arg.Float (function i -> temp_min := i), "Temperatura minima"    );
  ("-s" ,  Arg.Int   (function i -> semilla  := i), "Semilla para el PRG"   );
  ]

open Sqlite3

db_open "tsp"


