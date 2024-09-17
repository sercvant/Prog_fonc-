let rec bintodec(n, a) = 
    if n = 0 then 0
    else (n mod 10) *a + bintodec(n/10, a*2)

let bin2dec n = bintodec(n,1)

let () = let resultat = bin2dec 10101 in
Printf.printf "Le résultat est: %d\n" resultat 

