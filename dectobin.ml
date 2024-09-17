let rec dectobin (n, a) = 
    if n = 0 then 0
    else (n mod 2) *a + dectobin (n/2, a *10)

let dec2bin n = dectobin(n,1)

let () = let resultat = dec2bin 128 in
Printf.printf "Le résultat est: %d\n" resultat 


