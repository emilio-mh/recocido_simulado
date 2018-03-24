

### Recocido Simulado

Parámetros para obtener el resultado 0.262...
-ti 1.2
-tm 0.0001
-phi 0.99
-s 238776
-l 2000
-n 1

Instrucciones para compilar:

```markdown


apt-get install opam
opam switch 4.06.0
opam install ocamlbuild
opam install ocamlfind
opam install sqlite3


ocamlbuild -use-ocamlfind -pkgs sqlite3,str recocido.native




