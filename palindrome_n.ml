(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

let rec reverse_number n new_n = 
  if n = 0 then new_n 
  else reverse_number(n/10) (new_n * 10 + n mod 10)

let is_palindrome n = 
  n = reverse_number n 0

let () = 
  Printf.printf "est un paldindrome: %b\n" (is_palindrome 121)