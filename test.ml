#load "graphics.cma";;
open Graphics;;

open_graph "400x400";;
set_window_title "Demineur";;

(* Dimensions de la grille *)
let grid_rows = 10
let grid_cols = 10
let cell_size = 36

(* Type pour l'état de chaque cellule : cachée, révélée ou contenant une mine *)
type cell_state = Hidden | Revealed of int | Mine

(* Créer une matrice représentant l'état de chaque cellule de la grille *)
let grid = Array.make_matrix grid_rows grid_cols Hidden

(* Nombre de mines à placer dans la grille *)
let num_mines = 10

(* Fonction récursive pour placer des mines de manière aléatoire *)
let rec place_mines n =
  if n > 0 then
    let x = Random.int grid_cols in
    let y = Random.int grid_rows in
    if grid.(y).(x) != Mine then (
      grid.(y).(x) <- Mine;
      place_mines (n - 1)  (* Placer les mines restantes *)
    ) else
      place_mines n  (* Réessayer si une mine est déjà présente *)
  else ()

(* Fonction récursive pour compter le nombre de mines autour d'une cellule donnée *)
let rec count_adjacent_mines row col i j count =
  if i > 1 then count
  else
    let r = row + i in
    let c = col + j in
    let new_count =
      if r >= 0 && r < grid_rows && c >= 0 && c < grid_cols then
        match grid.(r).(c) with
        | Mine -> count + 1
        | _ -> count
      else count
    in
    if j < 1 then count_adjacent_mines row col i (j + 1) new_count
    else count_adjacent_mines row col (i + 1) (-1) new_count

(* Fonction pour dessiner la grille de manière récursive *)
let rec draw_grid_row i =
  if i < grid_rows then (
    let rec draw_grid_col j =
      if j < grid_cols then (
        let x = j * cell_size in
        let y = i * cell_size in
        set_color black;
        draw_rect x y cell_size cell_size;
        (* Si la cellule est révélée, on affiche le nombre de mines autour *)
        (match grid.(i).(j) with
        | Revealed mines ->
          set_color blue;
          fill_rect x y cell_size cell_size;
          set_color black;
          moveto (x + 10) (y + 15);
          draw_string (string_of_int mines)
        | _ -> ());
        draw_grid_col (j + 1)
      )
    in
    draw_grid_col 0;
    draw_grid_row (i + 1)
  )

let draw_grid () = draw_grid_row 0

(* Fonction pour gérer un clic sur une cellule *)
let rec handle_click1 x y =
  let col = x / cell_size in
  let row = y / cell_size in
  if row < grid_rows && col < grid_cols then (
    match grid.(row).(col) with
    | Hidden ->
      let mines_around = count_adjacent_mines row col (-1) (-1) 0 in
      grid.(row).(col) <- Revealed mines_around;
      Printf.printf "Cellule révélée : (%d, %d), Mines autour : %d\n" col row mines_around;
      draw_grid ()
    | Mine ->
      set_color red;
      let x = col * cell_size in
      let y = row * cell_size in
      fill_rect x y cell_size cell_size;
      set_color black;
      moveto (x + 10) (y + 15);
      draw_string "M";
      Printf.printf "Mine cliquée à : (%d, %d). Vous avez perdu !\n" col row;
      clear_graph ();
      moveto 150 200;
      draw_string "Game Over!";
      exit 0
    | Revealed _ -> ()
  )

(* Fonction récursive pour vérifier si un clic est dans un bouton du menu *)
let rec is_inside_button x y button_x button_y button_width button_height =
  x >= button_x && x <= (button_x + button_width) &&
  y >= button_y && y <= (button_y + button_height)

(* Fonction pour gérer les clics dans le menu *)
let rec handle_click x y =
  if is_inside_button x y 50 100 120 30 then
    (clear_graph ();
    moveto 50 200;
    place_mines num_mines;  (* Placer les mines aléatoirement *)
    draw_grid ())
  else if is_inside_button x y 50 150 120 30 then
    (clear_graph ();
    moveto 50 200;
    draw_string "Mode deux joueurs démarré !")
  else if is_inside_button x y 50 200 120 30 then
    (clear_graph ();
    moveto 50 200;
    draw_string "Options sélectionnées")

(* Fonction récursive pour gérer l'attente des clics *)
let rec wait_for_click () =
  let event = wait_next_event [Button_down] in
  let x = event.mouse_x in
  let y = event.mouse_y in
  handle_click x y;
  handle_click1 x y;
  wait_for_click ()

(* Afficher le menu principal de manière récursive *)
let rec draw_menu_button text y_offset =
  moveto 50 y_offset;
  draw_rect 50 y_offset 120 30;
  moveto 60 (y_offset + 15);
  draw_string text

let rec menu () =
  clear_graph ();
  set_color black;
  draw_menu_button "Jouer seul" 100;
  draw_menu_button "Jouer à deux" 150;
  draw_menu_button "Options" 200

(* Appel du menu et des événements *)
let () =
  menu ();
  wait_for_click ()
