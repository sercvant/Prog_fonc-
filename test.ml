#load "graphics.cma";;
open Graphics;;
open_graph "400x400";;
set_window_title "Demineur";;

(* Fonction pour afficher le menu *)
let menu () =
  clear_graph ();

  
  
  (* Affichage du bouton "Jouer seul" *)
  set_color black;
  moveto 50 100;
  draw_rect 50 100 120 30;
  moveto 60 115;
  draw_string "Jouer seul";

  (* Affichage du bouton "Jouer à deux" *)
  set_color black;
  moveto 50 150;
  draw_rect 50 150 120 30;
  moveto 60 165;
  draw_string "Jouer à deux";

  (* Affichage du bouton "Options" *)
  set_color black;
  moveto 50 200;
  draw_rect 50 200 120 30;
  moveto 60 215;
  draw_string "Options";;

(* Fonction pour tester les événements du menu *)
let rec test_menu () =
  let valeur = wait_next_event [Button_up; Key_pressed] in
  if valeur.keypressed && valeur.key = '\027' then exit 0 (* ESC pour quitter *)
  else test_menu ();;

let grid_rows = 10;;
let grid_cols = 10;;
let cell_size = 36;;
type cell_state = Hidden | Revealed

(* Créer une matrice représentant l'état de chaque cellule de la grille *)
let grid = Array.make_matrix grid_rows grid_cols Hidden

  let draw_grid () =
    for i = 0 to grid_rows - 1 do
      for j = 0 to grid_cols - 1 do
        (* Dessiner chaque cellule *)
        set_color black;
        let x = j * cell_size in
        let y = i * cell_size in
        draw_rect x y cell_size cell_size;
        (* Si la cellule est révélée, on la remplit *)
      if grid.(i).(j) = Revealed then (
        set_color red;
        fill_rect x y cell_size cell_size;
        set_color black;
        moveto (x + 10) (y + 15);
        draw_string "X"  (* Remplacez par une action spécifique si nécessaire *)
      )
    done;
  done

  (* Fonction pour déterminer si un clic est dans une cellule *)
let is_inside_cell x y cell_x cell_y cell_size =
  x >= cell_x && x <= (cell_x + cell_size) &&
  y >= cell_y && y <= (cell_y + cell_size)


(* Appel du menu et des événements *)
(* Vérifier si un clic est dans la zone d'un bouton *)
let is_inside_button x y button_x button_y button_width button_height =
  x >= button_x && x <= (button_x + button_width) &&
  y >= button_y && y <= (button_y + button_height)

  let handle_click1 x y =
    (* Convertir les coordonnées du clic en coordonnées de la grille *)
    let col = x / cell_size in
    let row = y / cell_size in
    if row < grid_rows && col < grid_cols then (
      (* Révéler la cellule si elle est encore cachée *)
      if grid.(row).(col) = Hidden then (
        grid.(row).(col) <- Revealed;
        Printf.printf "Cellule révélée : (%d, %d)\n" col row;
        draw_grid ()  (* Redessiner la grille après modification *)
      )
    )
(* Gérer les clics sur les boutons individuels *)
let handle_click x y =
  if is_inside_button x y 50 100 120 30 then
    (* Si le bouton "Jouer seul" est cliqué *)
    (clear_graph ();
    moveto 50 200;
    draw_grid())
  else if is_inside_button x y 50 150 120 30 then
    (* Si le bouton "Jouer à deux" est cliqué *)
    (clear_graph ();
    moveto 50 200;
    draw_string "Mode deux joueurs démarré !")
  else if is_inside_button x y 50 200 120 30 then
    (* Si le bouton "Options" est cliqué *)
    (clear_graph ();
    moveto 50 200;
    draw_string "Options sélectionnées")

(* Boucle pour attendre et gérer les clics *)
let rec wait_for_click () =
  let event = wait_next_event [Button_down] in
  let x = event.mouse_x in
  let y = event.mouse_y in
  handle_click x y;
  handle_click1 x y;
  wait_for_click ()

(* Appel du menu et des événements *)
let () =
  menu ();
  wait_for_click ()