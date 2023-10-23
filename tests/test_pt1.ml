(* TESTS DES FONCTIONS DE LA PARTIE 1 DU SUJET *)
(* Tests à copier dans un interpréteur Ocaml et à exécuter *)


(*
  type ruban : type qui représente le ruban infini de l'automate
   *)
   type ruban = {
    (* On écrase le premier caractère de right *)
    left: char list; (* l'élément en fin de liste correspond au premier elem la liste*)
    right: char list;
  }
(* ruban correspondant à "abcdef"*)
let test_rub = {left = ['c';'b';'a']; right = ['d';'e';'f']}


(* --------------------- test de move_left ----------------------- *)
(*
  [move_left] : ruban -> ruban
  @requires : un ruban valide
  @ensures : bouge le curseur d'un rang vers la gauche. Si left est vide, on ajoute un espace devant right
  @raises : nothing
*)
let move_left r = match r.left with
  | [] -> {left = []; right = (' ')::r.right}
  | h::t -> {left = t; right = h::r.right}

let test_move_left = move_left test_rub (* = {left = ['b';'a']; right = ['c';'d';'e';'f']} *)
let test2_move_left = move_left {left = []; right = ['a';'b']} (* = {left = []; right = [' ';'a';'b']} *)


(* ----------------------- test de move_right ---------------------- *)
(*
  [move_left] : ruban -> ruban
  @requires : un ruban valide
  @ensures : bouge le curseur d'un rang vers la droite. Si right est vide, on ajoute un espace devant left
  @raises : nothing
*)
let move_right r = match r.right with
  | [] -> {left = (' ')::r.left; right = []}
  | h::t -> {left = h::r.left; right = t}

let test_move_right = move_right test_rub (* = {left = ['c';'b';'a']; right = ['e';'f']} *)
let test2_move_right = move_right {left = ['b';'a']; right = []} (* = {left = [' ';'b';'a']; right = []} *)


(* ------------------ test de execute_program v1 ---------------------- *)
(* compiler et rentrer ./projet 1 message_3aien.prog 
   ce message contient tous les types d'instructions de cette partie
   il est donc un bon test de l'ensemble des fonctions *)
(* de même avec hello_left.prog *)


(* test de fold_ruban *)
(* quand on execute les commandes d'avant, le message s'affiche bien donc la fonction marche *)
