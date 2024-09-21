

let rec addition nombre puissance = 
  if puissance = 1 then nombre
  else addition (nombre+nombre) (puissance-1)

let getResult nombre puissance = addition nombre puissance

let () = 
  Printf.printf "Résultat: %d" (getResult 2 4)
  
  