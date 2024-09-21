(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

let rec addition nombre puissance = 
  if puissance = 1 then nombre
  else addition (nombre+nombre) (puissance-1)

let getResult nombre puissance = 
  if puissance = 0 then 1
  else
    addition nombre puissance
    

let () = 
  for var = 0 to 10 do
    Printf.printf "Résultat: %d\n" (getResult 2 var)
  done
  
  
  