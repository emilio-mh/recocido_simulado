

let barridoaux (s : int array ) (costo : int array -> float) : int array = 
  let mejor_costo = ref (costo s) in
  let mejor_vecino = ref s in
  for i = 0 to (Array.length s) - 1 do 
    for j = i + 1 to (Array.length s) - 1 do
      let tmp = Array.get s i in
      Array.set s i (Array.get s j);
      Array.set s j tmp;
      let fs = (costo s) in
      if (fs < !mejor_costo) then (
        mejor_costo := fs;
        mejor_vecino := (Array.copy s);
      );
      Array.set s j (Array.get s i);
      Array.set s i tmp;
    done;
  done; !mejor_vecino

let rec barrido (s : int array ref) (costo : int array -> float) : int array = 
  let nv = ref (barridoaux (Array.copy !s) costo) in
  let fs = ref (costo !s) in
  let fnv = ref (costo !nv) in
  while fnv < fs do
    s  := !nv;
    fs := !fnv;
    nv := barridoaux !nv costo ;
    fnv := costo !nv
  done; !nv

let barrido3aux (s : int array) (costo : int array -> float) = 
  let mejor_costo = ref (costo s) in
  let mejor_vecino = ref s in
  for i = 0 to (Array.length s) - 1 do 
    for j = i + 1 to (Array.length s) - 1 do
      for k = j + 1 to (Array.length s) - 1 do
        let tmpi = Array.get s i in
        let tmpj = Array.get s j in
        let tmpk = Array.get s k in
        Array.set s i tmpk;
        Array.set s j tmpi;
        Array.set s k tmpj;

        let fs = ref (costo s) in
        if (!fs < !mejor_costo) then (
          mejor_costo := !fs;
          mejor_vecino := (Array.copy s);
        );

        Array.set s i tmpj;
        Array.set s j tmpk;
        Array.set s k tmpi;

        fs := costo s;
        if (!fs < !mejor_costo) then (
          mejor_costo := !fs;
          mejor_vecino := (Array.copy s);
        );

        Array.set s i tmpi;
        Array.set s j tmpj;
        Array.set s k tmpk;
        
      done;
    done;
  done; !mejor_vecino
  

let barrido3 (s : int array ref) (costo : int array -> float) =
  let nv = ref (barrido3aux (Array.copy !s) costo) in
  let fs = ref (costo !s) in
  let fnv = ref (costo !nv) in
  while !fnv < !fs do
    s := !nv;
    fs := !fnv;
    nv := barrido3aux !nv costo;
    fnv := costo !nv
  done; !nv 

