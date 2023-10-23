(* --------------------------- test de caesar_list ----------------------------- *)
(*
  [f] : char list -> char -> int -> char list
  @requires : nothing
  @ensures : crypte le caractère c avec le décalage n
  @raises : nothing
*)
let f acc c n =
  if n >= 0 then
    let is_letter = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in (* on vérifie que c est une lettre *)
    if is_letter then (* si c'est une lettre ...*)
      let is_upper = (c >= 'A' && c <= 'Z') in (* on regarde si c'est une majuscule ou une minuscule *)
      let offset = if is_upper then Char.code 'A' else Char.code 'a' in (* On récupère l'entier codant 'A' ou 'a' ou a en ASCII *)
      let c_index = (Char.code c) - offset in (* On "ramène le problème en 0" en fonction de la nature de c *)
      let new_index = (c_index + n) mod 26 in (* mod 26 pour retomber sur un nombre entre 0 et 25 *)
      (Char.chr (new_index + offset)) :: acc (* on retourne à la "vraie" valeur de notre caractère crypté en redéclant de offset *)
    else (* sinon on ne le transforme pas *)
      c :: acc
  else (* si n est négatif, on fait la même chose mais en décalant vers la gauche *)
    let is_letter = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
    if is_letter then
      let is_upper = (c >= 'A' && c <= 'Z') in
      let offset = if is_upper then Char.code 'A' else Char.code 'a' in
      let c_index = (Char.code c) - offset in
      let new_index = (c_index + n + 26) mod 26 in (* on ajoute 26 pour éviter les problèmes de négatifs *)
      (Char.chr (new_index + offset)) :: acc
    else
      c :: acc

(*
  [caesar_list] : int -> char list -> char list
  @requires : nothing
  @ensures : crypte la liste de caractères l avec le décalage n
  @raises : nothing
*)
let caesar_list n l = 
  (* Un fold_right permet d'obtenir la liste des char codés non inversée *)
  List.fold_right (fun c -> fun acc -> f acc c n) l []


let l_test_1 = ['H';'e';'l';'l';'o';' ';'W';'o';'r';'l';'d';'!']
let res = caesar_list 3 l_test_1 (* = ['K'; 'h'; 'o'; 'o'; 'r'; ' '; 'Z'; 'r'; 'u'; 'o'; 'g'; '!'] *)

(* ---------------------------------- test de suppr_char_list --------------------------------- *)
(*
  [suppr_char_list] : char -> char list -> char list
  @requires : nothing
  @ensures : supprime toutes les occurences de c dans l
  @raises : nothing
*)
let rec suppr_char_list c l = match l with
  | [] -> l
  | h::t -> if (c = h) then (suppr_char_list c t) else (h::(suppr_char_list c t))

let l_test_2 = ['a';'a';'c';'d';'a';'e']
let test_suppr_char_list = suppr_char_list 'a' l_test_2 (* = ['c';'d';'e'] *)

(* ----------------------- test de la fonctionnalité Invert --------------------------------- *)
(* compilez le projet, executer ./projet 2 test_invert_i.prog avec i = 1, 2, les résultats 
   attendus sont indiqués dans les fichiers correspondant *)