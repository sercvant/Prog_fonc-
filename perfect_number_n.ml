(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

let rec somme_diviseur n diviseur somme = 
  if diviseur = n -1 then somme
  else if n mod diviseur = 0 then 
    somme_diviseur n (diviseur+1) (somme+diviseur)
  else
    somme_diviseur n (diviseur+1) somme

let getSommeDiviseur n = 
  somme_diviseur n 1 0
  
let () = 
  let nombre = 18 in 
  let value = getSommeDiviseur nombre in 
  if value = nombre then 
    Printf.printf "%d est parfait\n" nombre
  else
    Printf.printf "%d n'est pas parfait\n" nombre
      
  
  
  