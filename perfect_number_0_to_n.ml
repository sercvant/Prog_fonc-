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
  if n = 0 then 1
  else if n = 1 then 0
  else
    somme_diviseur n 1 0
  
let () = 
  for var = 0 to 30 do
    let value = getSommeDiviseur var in 
    if value = var then 
      Printf.printf "%d est parfait\n" var
    else
      Printf.printf "%d n'est pas parfait\n" var
  done
      
  
  
  