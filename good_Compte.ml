
let soi (i:int) = string_of_int i (* string of int mais plus rapide a écrire *)

let min a b =
  if a > b then b else a

let max a b =
  if a > b then a else b

let remove_indices mat i j operation = (* cette fonction recopie un tableau mais remplace l'indice i par la valeure opération et omet l'indice j *)
  let n = Array.length mat in
  let a = min i j in
  let b = max i j in
  Array.init ( n - 1 ) ( fun k ->
    if k = a then operation
    else if k < b then mat.( k )
    else mat.( k + 1 ) )

let solveur a b c d e f num = (* cette fonction resoud la partie*)
  let l = [| (a, soi a); (b, soi b); (c, soi c); (d, soi d); (e, soi e); (f, soi f) |] in
  let memo = Hashtbl.create 1000 in

  let rec solve elements =
    let n = Array.length elements in

    if n = 1 then 
      let (x, y) = elements.(0) in
      if x = num then y else "impossible"
      
    else (
      let key = (*cette clee est le fruit de recherches sur internet, notament pour fold_left, et j'ai eu l'idee de trier les elements pour ameliorer la table *)
        let sorted_elements = Array.copy elements in
        Array.sort (fun (x1, _) (x2, _) -> max x1 x2) sorted_elements; (* facon rapide de trier, trouvé sur internet celle la je la comprends*)
        Array.fold_left (fun acc (x, _) -> acc ^ soi x ^ "-") "" sorted_elements in (* Crée la clé à partir des éléments triés , je ne comprend pas le fonctionnement pour etre honnete *)
      
      if Hashtbl.mem memo key then 
        Hashtbl.find memo key (*si la solution est deja enregistrée, on la renvoie directement et on evite des calculs *)
      else (
        let rec verify i =
          if i >= n then "non"
          else
            let (x, y) = elements.(i) in
            if x = num then y
            else verify (i + 1) in
        
        let find = verify 0 in
        if find <> "non" then find

        else
          let result = ref "impossible" in (* on stockera le resultat avec un pointeur, je ne sias pas pourquoi cela ne fonctionnait pas sans ça*)
          for i = 0 to n - 2 do
            for j = i + 1 to n - 1 do
              let (x1, y1) = elements.(i) in
              let (x2, y2) = elements.(j) in
              let a1 = (x1 + x2, y1 ^ " + " ^ y2) in
              let a2 = (x1 * x2, "( " ^ y1 ^ " ) * ( " ^ y2 ^ " )") in
              let a3 = (x1 - x2, y1 ^ " - ( " ^ y2 ^ " )") in
              let a4 = (x2 - x1, y2 ^ " - ( " ^ y1 ^ " )") in

              if x1 <> 0 then (* on n'omet pas les disjonctions de cas pour eviter les divisions par 0 avec lopérateur /*)
                let a5 = (x2 / x1, "( " ^ y2 ^ " ) / ( " ^ y1 ^ " )") in
                let b5 = solve (remove_indices elements i j a5) in
                if b5 <> "impossible" then result := b5;
              if x2 <> 0 then
                let a6 = (x1 / x2, "( " ^ y1 ^ " ) / ( " ^ y2 ^ " )") in
                let b6 = solve (remove_indices elements i j a6) in
                if b6 <> "impossible" then result := b6;

              let b1 = solve (remove_indices elements i j a1) in
              let b2 = solve (remove_indices elements i j a2) in
              let b3 = solve (remove_indices elements i j a3) in
              let b4 = solve (remove_indices elements i j a4) in

              if b1 <> "impossible" then result := b1
              else if b2 <> "impossible" then result := b2
              else if b3 <> "impossible" then result := b3
              else if b4 <> "impossible" then result := b4;
            done;
          done;

          Hashtbl.add memo key !result;
          !result
      )
    )
  in
  solve l
