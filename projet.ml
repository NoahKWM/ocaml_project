
(** 
    Cette partie du programme NE DOIT EN AUCUN CAS être modifiée sous peine
    de voir votre code ne pas passer les tests 
*)
type instruction = (* représentation d'une instruction reçu *)
  | Left (* Déplacement du curseur vers la gauche *)
  | Right (* Déplacement du curseur vers la droite *)
  | Write of char (* Écriture du caractère sur le ruban *)
  | Repeat of (int * instruction list) (* Repeter n fois la liste d'instructions *)
  | Caesar of int (* Caesar n à partir de la phase 3Aiens *)
  | Delete of char (* Delete(a) suppression du caractère a du message à partir de la phase 3Aiens *)
  | Invert (* Invert : retournement du message  à partir de la phase 3Aiens *)
  
type program = instruction list (* Un programme est simplement une liste d'instruction *)

(**
   VOUS N'AVEZ PAS BESOIN DE LIRE OU DE COMPRENDRE CETTE PARTIE DU CODE (LES SIX FONCTIONS SUIVANTES). 
   ELLE EST VOLONTAIREMENT NON COMMENTÉE ET RÉDIGÉE DE MANIÈRE PEU CLAIRE 
 *)
let rec analyse_comment fd =
  let c = Scanf.bscanf fd "%c" (fun c -> c) in
  if c = '\n'
  then ()
  else analyse_comment fd
             
let rec analyse_program_aux =
  fun fd lvl  ->
  try 
    let c = Scanf.bscanf fd "%c" (fun a  -> a) in
    if c = '#'
    then
      let _ = analyse_comment fd in
      analyse_program_aux fd lvl
    else if c = ';' || c = '\n' || c = ' '
    then analyse_program_aux fd lvl
    else if c = ']'
    then
      if lvl > 0
      then []
      else raise (Invalid_argument "Error on char ]")
    else if c = 'W'
      then
        let i= Write(Scanf.bscanf fd "(%c)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'C'
      then
        let i= Caesar(Scanf.bscanf fd "(%d)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
    else if c = 'D'
    then
      let a = Scanf.bscanf fd "(%c)" (fun a -> a) in 
      let i= Delete(a) in
      let li = analyse_program_aux fd lvl in
      i::li
    else if c = 'R'
    then let li = analyse_program_aux fd lvl in
         Right::li
    else if c = 'I'
    then Invert::analyse_program_aux fd lvl
    else if c = 'L'
    then Left::analyse_program_aux fd lvl
    else if c = 'F'
    then
      let n = Scanf.bscanf fd "(%d,[" (fun n -> n) in
      let l = analyse_program_aux fd (lvl + 1) in
      let c = Scanf.bscanf fd "%c" (fun a -> a) in
      if c <> ')' then raise (Invalid_argument ("Error found '"^String.make 1 c^"' instead of ')'"))
      else
        let li = analyse_program_aux fd lvl in
        Repeat(n,l)::li
    else
      let _ = Format.printf  "ERROR '%c' (%d)@." c (Char.code c) in
      assert false
  with End_of_file -> []

let rec read_file_aux =
  fun acc fd ->
  try
    let c = Scanf.bscanf fd "%c" (fun x -> x) in
    read_file_aux (c::acc) fd
  with End_of_file -> acc

let read_file file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      List.rev (read_file_aux [] fd)
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127

                    
let rec fprintf_instruction fmt i =
  match i with
  | Write c -> Format.fprintf fmt "W(%c)" c
  | Right -> Format.fprintf fmt "R"
  | Left -> Format.fprintf fmt "L"
  | Repeat(n,li) ->
     Format.fprintf fmt "F(%d,[%a])" n (fun fmt li -> List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) li) li
  | Caesar n -> Format.fprintf fmt "C(%d)" n
  | Delete(a) -> Format.fprintf fmt "D(%c)" a
  | Invert -> Format.fprintf fmt "I"
            
let fprintf_program fmt l =
  List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) l

(*** 
     Retour au côté clair
*)


(* 
   [print_program] : program -> unit 
   affiche un [program] sur la sortie standard de l'executable
 *)  
let print_program p = Format.printf "%a" fprintf_program p

(*
  [analyse_program] : unit -> program
  Lit un programme 1Aien ou 2Aien, l'analyse et le retourne sous forme d'une valeur de type [program]
 *)                    
let analyse_program file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      analyse_program_aux fd 0
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127









    
(** Votre code doit commencer à partir de ce point.

    NE MODIFIER RIEN AU DESSUS DE CE POINT 
*)

(*
  type ruban : type qui représente le ruban infini de l'automate
   *)
type ruban = {
  (* On écrase le premier caractère de right *)
  left: char list; (* l'élément en fin de liste correspond au premier elem la liste*)
  right: char list;
}

(*
  [move_left] : ruban -> ruban
  @requires : un ruban valide
  @ensures : bouge le curseur d'un rang vers la gauche. Si left est vide, on ajoute un espace devant right
  @raises : nothing
*)
let move_left r = match r.left with
  | [] -> {left = []; right = (' ')::r.right}
  | h::t -> {left = t; right = h::r.right}

(*
  [move_left] : ruban -> ruban
  @requires : un ruban valide
  @ensures : bouge le curseur d'un rang vers la droite. Si right est vide, on ajoute un espace devant left
  @raises : nothing
*)
let move_right r = match r.right with
  | [] -> {left = (' ')::r.left; right = []}
  | h::t -> {left = h::r.left; right = t}

(*
  [suppr_char_list] : char -> char list -> char list
  @requires : nothing
  @ensures : supprime toutes les occurences de c dans l
  @raises : nothing
*)
let rec suppr_char_list c l = match l with
  | [] -> l
  | h::t -> if (c = h) then (suppr_char_list c t) else (h::(suppr_char_list c t))

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

(*
  [execute_program] : program -> ruban
  @requires : un program p valide
  @ensures : renvoie un ruban décodant le programme p
  @raises : nothing
*)
let execute_program p =
  let r = {left = []; right = []} in (* initialisation du ruban *)
  let rec aux prog rub = (* fonction auxiliaire récursive permettant de parcourir le programme p *)
    match prog with (* on match sur la structure du programme pour identifier les instructions *)
    | [] -> rub
    | Left::t -> aux t (move_left rub)
    | Right::t -> aux t (move_right rub)
    | Write(c)::t -> aux t ( match rub.right with (* le comportement diffère en fonction de la structure de right *)
          | [] -> {left = rub.left; right = [c]} (* si right est vide on écrit simplement le caractère dedans *)
          | t1::q1 -> {left = rub.left; right = c::q1} ) (* sinon on écrase la tête *)
    | Repeat(n, instruc)::t -> if (n = 0) then (aux t rub) (* si n est nul, on ignore Repeat *)
                               else (aux (Repeat(n-1, instruc)::t) (aux instruc rub)) (* sinon on rappelle repeat n-1 fois sur le ruban modifié par une première série d'instruction *)
    | Caesar(n)::t -> aux t ({left = caesar_list n rub.left; right = caesar_list n rub.right}) (* on crypte les données des deux list du ruban à l'aide de caesar_list *)
    | Delete(a)::t -> aux t ({left = (suppr_char_list a rub.left); right = (suppr_char_list a rub.right)}) (* supprime toutes les occurences de a dans left et right *)
    | Invert::t -> aux t (move_left {left = rub.right; right = rub.left} ) (* inverse left et right, le dernier caractères de left avant inversion est bien le dernier caractère du ruban *)
                                                                           (*de plus, on déplace le curseur vers la gauche pour le replacer sur l'ancien caracère courant *)
  in
  aux p r

(*
  [fold_ruban] : ('a -> char -> 'a) -> 'a -> ruban -> 'a
  @requires : nothing
  @ensures : parcourt le ruban de gauche à droite en lui appliquant f
  @raises : nothing
*)
let fold_ruban f v0 r = 
  let acc = List.fold_right (fun e -> fun acc -> f acc e) r.left  v0 in (* fold_right pour left car left est inversée /!\ *)
  List.fold_left f acc r.right (* fold_left sur right *)

(*
  [generate_program] : string -> program
  @requires : nothing
  @ensures : génère un programme permettant de coder le message msg
  @raises : nothing
*)
let generate_program msg =
  let rec count_repeats m count c = match m with
    | [] -> (count, [])
    | c'::t' -> if c' = c then
                  count_repeats t' (count + 1) c
                else
                  (count, m)
  in
  let rec aux m acc = match m with
    | [] -> acc
    | c::t -> let count, rest = count_repeats t 1 c in
              if count > 1 then
                  aux rest ((Repeat(count, [Write c; Right]))::acc)
              else
                  aux rest ((Right)::(Write c)::acc)
  in
  List.rev (aux msg [])

(** Votre code doit s'arreter à partir de ce point.

    NE MODIFIER RIEN EN DESSOUS DE CE POINT 
 *)











                         
                         
let die i = 
  let _ = Format.fprintf Format.err_formatter "Usage: %s <1|2|3> <file>@." Sys.argv.(0) in
  exit i
  
let main phase file =
  if phase = "1" || phase = "2"
  then
    let li = analyse_program file in
    let rub = execute_program li in
    let _ = fold_ruban (fun _ c -> Format.printf "%c" c) () rub in
    Format.printf "@."
  else if phase = "3"
  then
    let msg = read_file file in
    let p = generate_program msg in
    print_program p
  else die 1


let _ =
  if Array.length Sys.argv = 3 
  then
    main Sys.argv.(1) Sys.argv.(2)
  else die 2
