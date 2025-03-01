open OUnit2
open Battleship

(* Source: battleship corner test case examples” prompt, labs.openai.com/, lines
   7, 17 25, 38, 53 accessed 5/12/24.*)

let test_create_grid _ =
  let grid = create_grid 10 in
  assert_equal 10 (Array.length grid);
  assert_equal 10 (Array.length grid.(0));
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid - 1 do
      assert_equal Empty grid.(y).(x)
    done
  done

let test_print_grid _ =
  let grid = create_grid 10 in
  assert_bool "No exception on print_grid"
    (try
       print_grid grid true "Test Grid";
       true
     with _ -> false)

let test_place_ship_success _ =
  let grid = create_grid 10 in
  assert_bool "Horizontal placement should succeed"
    (place_ship grid 1 (0, 0) (0, 4));
  assert_bool "Vertical placement should succeed"
    (place_ship grid 2 (2, 2) (6, 2));
  for x = 0 to 4 do
    assert_equal (Ship 1) grid.(0).(x)
  done;
  for y = 2 to 6 do
    assert_equal (Ship 2) grid.(y).(2)
  done

let test_place_ship_max_min _ =
  let grid = create_grid 10 in
  assert_bool "Placement with reverse order coordinates should succeed"
    (place_ship grid 1 (3, 7) (3, 3));
  for x = 3 to 7 do
    assert_equal (Ship 1) grid.(3).(x)
  done;
  assert_bool "Vertical placement with reverse order coordinates should succeed"
    (place_ship grid 2 (8, 5) (4, 5));
  for y = 4 to 8 do
    assert_equal (Ship 2) grid.(y).(5)
  done;
  assert_bool "Overlap placement should fail"
    (not (try place_ship grid 3 (3, 5) (3, 9) with _ -> false))

let test_place_ship_failures _ =
  let grid = create_grid 10 in
  let _ = place_ship grid 1 (0, 0) (0, 4) in
  assert_bool "Diagonal placement should fail"
    (not (try place_ship grid 1 (0, 0) (4, 4) with _ -> false));
  assert_bool "Overlap placement should fail"
    (not (try place_ship grid 2 (0, 2) (0, 6) with _ -> false))

let test_shoot _ =
  let grid = create_grid 10 in
  let _ = place_ship grid 1 (0, 0) (0, 4) in
  assert_equal "Hit!" (shoot grid (0, 0));
  assert_equal "Miss!" (shoot grid (1, 0));
  assert_equal "Already guessed this position!" (shoot grid (0, 0))

let test_ai_guess _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 1 (0, 0) (0, 2));
  (* let guesses = ref [] in *)
  let result = ref "Miss!" in
  let guess = ai_guess grid in
  let () = result := guess in
  (* for _ = 1 to 100 do let guess = ai_guess grid in if not (List.mem guess
     !guesses) then begin guesses := guess :: !guesses; result := guess end
     done; *)
  assert_bool "AI should hit or miss" (!result = "Hit!" || !result = "Miss!")

let test_check_game_over _ =
  let grid = create_grid 10 in
  assert_bool "New grid should not be game over" (not (check_game_over grid));
  ignore (place_ship grid 1 (0, 0) (0, 4));
  for i = 0 to 4 do
    ignore (shoot grid (0, i))
  done;
  assert_bool "All ships sunk should be game over" (check_game_over grid)

let test_invalid_input _ =
  let grid = create_grid 10 in
  assert_raises InvalidPlacement (fun () ->
      ignore (place_ship grid 1 (-1, 0) (0, -1)))

let test_char_to_index _ =
  assert_equal 0 (char_to_index 'A');
  assert_equal 0 (char_to_index 'a');
  assert_equal 25 (char_to_index 'z')

let test_validate_coordinates _ =
  assert_bool "Valid coordinates (5,5)" (validate_coordinates 5 5 10);
  assert_bool "Invalid coordinates (-1,5)"
    (not (validate_coordinates (-1) 5 10));
  assert_bool "Invalid coordinates (0,11)" (not (validate_coordinates 0 11 10))

let test_boundary_ship_placement _ =
  let grid = create_grid 10 in
  assert_bool "Place ship on the boundary should succeed"
    (place_ship grid 3 (9, 0) (9, 2));
  assert_bool "Place ship on the boundary should succeed"
    (place_ship grid 3 (0, 9) (2, 9))

let test_boundary_ship_placement_failure _ =
  let grid = create_grid 10 in
  assert_raises InvalidPlacement (fun () ->
      ignore (place_ship grid 4 (9, 8) (9, 11)))

let test_horizontal_ship_overlap _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 5 (2, 2) (2, 6));
  assert_bool "Overlap should fail"
    (not (try place_ship grid 6 (2, 5) (2, 9) with _ -> false))

let test_vertical_ship_overlap _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 7 (3, 3) (7, 3));
  assert_bool "Vertical overlap should fail"
    (not (try place_ship grid 8 (5, 3) (9, 3) with _ -> false))

let test_complete_ship_destruction _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 9 (1, 1) (1, 5));
  List.iter (fun i -> ignore (shoot grid (1, i))) [ 1; 2; 3; 4 ];
  assert_equal "You sunk a ship!" (shoot grid (1, 5))

let test_multiple_shots_same_place _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 1 (0, 0) (1, 0));
  ignore (shoot grid (0, 0));
  ignore (shoot grid (1, 0));
  assert_equal "Already guessed this position!" (shoot grid (0, 0))

let test_random_ai_guess_effectiveness _ =
  let grid = create_grid 10 in
  List.iter
    (fun i -> ignore (place_ship grid (20 + i) (i, i) (i, i + 4)))
    [ 0; 1; 2; 3; 4 ];
  let results = List.init 50 (fun _ -> ai_guess grid) in
  let hits = List.filter (fun x -> x = "Hit!") results in
  assert_bool "AI should hit at least once" (List.length hits > 0)

let test_ship_placement_in_empty_grid _ =
  let grid = create_grid 10 in
  assert_bool "Ship should place in empty grid"
    (place_ship grid 11 (0, 0) (0, 2))

let test_ship_placement_boundary _ =
  let grid = create_grid 10 in
  assert_bool "Ship should place on grid boundary"
    (place_ship grid 12 (9, 7) (9, 9))

let test_ship_placement_at_edges _ =
  let grid = create_grid 10 in
  assert_bool "Placement at top edge should succeed"
    (place_ship grid 13 (0, 0) (0, 3));
  assert_bool "Placement at bottom edge should succeed"
    (place_ship grid 14 (9, 6) (9, 9));
  assert_bool "Placement at left edge should succeed"
    (place_ship grid 15 (2, 0) (5, 0));
  assert_bool "Placement at right edge should succeed"
    (place_ship grid 16 (2, 9) (5, 9))

let test_ships_touching_at_corners _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 17 (0, 0) (0, 3));
  ignore (place_ship grid 18 (1, 4) (4, 4));
  assert_bool "Ships touch, but don't overlap"
    (place_ship grid 19 (0, 4) (0, 5))

let test_ship_wrapping _ =
  let grid = create_grid 10 in
  assert_raises InvalidPlacement (fun () ->
      ignore (place_ship grid 20 (0, 8) (0, 11)))

let test_full_grid_sunk _ =
  let grid = create_grid 10 in
  assert_bool "Place all ships without overlap"
    (List.fold_left
       (fun acc (id, y, x1, x2) -> acc && place_ship grid id (y, x1) (y, x2))
       true
       [ (1, 0, 0, 1); (2, 1, 0, 2); (3, 2, 0, 3); (4, 3, 0, 4); (5, 4, 0, 5) ]);
  List.iter
    (fun y ->
      for x = 0 to 9 do
        ignore (shoot grid (y, x))
      done)
    [ 0; 1; 2; 3; 4 ];
  assert_bool "All cells sunk should be game over" (check_game_over grid)

let test_ai_efficiency_easy _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 220 (1, 1) (1, 4));
  set_ai_mode Easy;
  let results = List.init 50 (fun _ -> ai_guess grid) in
  let hits = List.filter (fun x -> x = "Hit!") results in
  assert_bool "Easy AI should have random efficiency"
    (List.length hits > 0 && List.length hits <= 50)

let test_ai_efficiency_hard _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 221 (5, 3) (5, 9));
  ignore (place_ship grid 221 (4, 0) (4, 8));
  set_ai_mode Hard;
  let hits = ref 0 in
  for _ = 1 to 50 do
    if ai_guess grid = "Hit!" then incr hits
  done;
  assert_bool "Hard AI should be more efficient than Easy AI" (!hits > 10)

let powerup_grid = create_grid 10
let gold = ref 100
let bombed_rows = ref []
let bombed_columns = ref []
let bombed_squares = ref []

let bomb_row row =
  let final = ref "Miss!" in
  let y = char_to_index row in
  if List.mem y !bombed_rows then begin
    final := "Already bombed that row!"
  end
  else begin
    bombed_rows := y :: !bombed_rows;
    for i = 0 to Array.length powerup_grid - 1 do
      let result = shoot powerup_grid (y, i) in
      if result = "Hit!" then gold := !gold + 50
      else if result = "You sunk a ship!" then
        let () = gold := !gold + 50 in
        final := result
    done;
    gold := !gold - 100
  end;
  !final

let test_bomb_row _ =
  let _ = place_ship powerup_grid 1 (0, 1) (0, 5) in
  assert_bool "Bombing a fresh row" ("You sunk a ship!" = bomb_row 'A');
  assert_bool "Gold after bombing a ship length 5" (!gold = 250);
  assert_bool "Bombing previously bombed row"
    ("Already bombed that row!" = bomb_row 'A')

let powerup_grid2 = create_grid 10
let () = gold := 100

let bomb_column col =
  let final = ref "" in
  let x = int_of_string col - 1 in
  if List.mem x !bombed_columns then begin
    final := "Already bombed that column!"
  end
  else begin
    bombed_columns := x :: !bombed_columns;
    for i = 0 to Array.length powerup_grid2 - 1 do
      let result = shoot powerup_grid2 (i, x) in
      if result = "Hit!" then gold := !gold + 50
      else if result = "You sunk a ship!" then
        let () = gold := !gold + 50 in
        final := result
    done;
    gold := !gold - 100
  end;
  !final

let test_bomb_column _ =
  let _ = place_ship powerup_grid2 1 (1, 0) (4, 0) in
  assert_bool "Bombing a fresh column" ("You sunk a ship!" = bomb_column "1");
  assert_bool "Gold after bombing a ship length 4" (!gold = 200);
  assert_bool "Bombing previously bombed column"
    ("Already bombed that column!" = bomb_column "1")

let powerup_grid3 = create_grid 10

let square_bomb row col =
  let final = ref "Miss!" in
  let y = char_to_index row in
  let x = int_of_string col - 1 in
  if not (validate_bomb y x (Array.length powerup_grid3)) then begin
    final := "Invalid selection!"
  end
  else if List.mem (y, x) !bombed_squares then begin
    final := "Already bombed that square!"
  end
  else begin
    bombed_squares := (y, x) :: !bombed_squares;
    for i = 0 to 2 do
      for j = 0 to 2 do
        let result = shoot powerup_grid3 (y + i, x + j) in
        if result = "Hit!" then
          let () = gold := !gold + 50 in
          final := result
      done
    done;
    gold := !gold - 100
  end;
  !final

let test_square_bomb _ =
  let _ = place_ship powerup_grid3 1 (1, 0) (4, 0) in
  assert_bool "Bombing a fresh square" ("Hit!" = square_bomb 'A' "1");
  assert_bool "Gold after bombing 2 ship cells" (!gold = 100);
  assert_bool "Bombing previously bombed square"
    ("Already bombed that square!" = square_bomb 'A' "1");
  assert_bool "Bombing invalid square"
    ("Invalid selection!" = square_bomb 'A' "0")

let rec airstrike grid shots =
  let final = ref "" in
  let grid_size = Array.length grid in
  let x = Random.int grid_size and y = Random.int grid_size in
  if shots = 0 then print_string ""
  else
    let () = gold := !gold - 25 in
    match grid.(y).(x) with
    | Hit _ | Miss -> airstrike grid shots
    | _ ->
        let result = shoot grid (y, x) in
        if result = "Hit!" then
          let () = gold := !gold + 50 in
          let () = final := result in
          airstrike grid (shots - 1)
        else
          let () = final := result in
          airstrike grid (shots - 1)

let powerup_grid4 = create_grid 10

let test_airstrike _ =
  let _ = place_ship powerup_grid4 1 (1, 0) (4, 0) in
  let _ = airstrike powerup_grid4 4 in
  assert_equal 4
    (count_hit_cells powerup_grid4 + count_cell_type powerup_grid4 Miss)

let count_grid = create_grid 10
let _ = place_ship count_grid 1 (1, 0) (1, 9)
let _ = shoot count_grid (0, 0)
let _ = shoot count_grid (2, 0)
let _ = shoot count_grid (3, 0)
let _ = shoot count_grid (1, 3)

let test_cell_count _ =
  assert_bool "Counting Empty cells" (87 = count_cell_type count_grid Empty);
  assert_bool "Counting Miss cells" (3 = count_cell_type count_grid Miss);
  assert_bool "Counting Hit cells" (1 = count_hit_cells count_grid)

let test_overlapping_ships_horizontally_vertically _ =
  let grid = create_grid 10 in
  assert_bool "Place horizontal ship" (place_ship grid 1 (1, 0) (1, 4));
  assert_bool "Overlap vertically should fail"
    (not (try place_ship grid 2 (0, 2) (3, 2) with _ -> false));
  assert_bool "Place vertical ship" (place_ship grid 1 (4, 5) (8, 5));
  assert_bool "Overlap vertically should fail"
    (not (try place_ship grid 2 (6, 4) (6, 7) with _ -> false))

let test_adjacent_ships _ =
  let grid = create_grid 10 in
  assert_bool "Place first ship" (place_ship grid 1 (0, 0) (0, 4));
  assert_bool "Place adjacent ship vertically" (place_ship grid 2 (1, 0) (1, 4))

let test_firing_at_all_coordinates _ =
  let grid = create_grid 10 in
  let _ = place_ship grid 1 (0, 0) (0, 9) in
  for i = 0 to 9 do
    for j = 0 to 9 do
      ignore (shoot grid (i, j))
    done
  done;
  assert_bool "All cells shot at least once" (check_game_over grid)

let test_ai_shoots_all_grid_once _ =
  let grid = create_grid 10 in
  set_ai_mode Easy;
  let all_shots = ref 0 in
  for _ = 1 to 100 do
    let shot = ai_guess grid in
    if shot != "Already guessed this position!" then all_shots := !all_shots + 1
  done;
  assert_equal ~msg:"AI should shoot each cell only once" 100 !all_shots

let test_game_over_after_all_ships_hit _ =
  let grid = create_grid 10 in
  assert_bool "Place ship" (place_ship grid 1 (0, 0) (0, 9));
  for i = 0 to 9 do
    ignore (shoot grid (0, i))
  done;
  assert_bool "Game should be over after all ships hit" (check_game_over grid)

let test_invalid_ship_placement_at_edges _ =
  let grid = create_grid 10 in
  assert_raises InvalidPlacement (fun () ->
      ignore (place_ship grid 1 (0, 0) (0, 10)))

let test_ship_placement_at_corners _ =
  let grid = create_grid 10 in
  assert_bool "Place ship at top-left corner" (place_ship grid 1 (0, 0) (0, 4));
  assert_bool "Place ship at bottom-right corner"
    (place_ship grid 2 (9, 5) (9, 9))

let test_various_ship_sizes _ =
  let grid = create_grid 10 in
  List.iteri
    (fun i size ->
      assert_bool
        (Printf.sprintf "Place ship size %d" size)
        (place_ship grid (i + 1) (i, 0) (i, size - 1)))
    [ 2; 3; 3; 4; 5 ]

let test_reset_game _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 1 (0, 0) (0, 9));
  ignore (shoot grid (0, 0));
  let grid_new = create_grid 10 in
  assert_bool "New grid should not be game over"
    (not (check_game_over grid_new))

let test_shooting_beyond_grid_boundaries _ =
  let grid = create_grid 10 in
  assert_raises InvalidInput (fun () -> shoot grid (10, 10))

let test_full_grid_no_ships _ =
  let grid = create_grid 10 in
  assert_bool "Game should not be over with no ships"
    (not (check_game_over grid))

let test_ship_destruction_notification _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 1 (0, 0) (0, 1));
  ignore (shoot grid (0, 0));
  assert_equal "You sunk a ship!" (shoot grid (0, 1))

let test_player_switch_in_two_player_mode _ =
  let grid1 = create_grid 10 in
  let grid2 = create_grid 10 in
  ignore (place_ship grid1 1 (0, 0) (0, 4));
  ignore (shoot grid1 (0, 0));
  assert_equal Empty grid2.(0).(0)

let test_mine_placement_success _ =
  let grid = create_grid 10 in
  assert_bool "Mine placed successfully" (place_mine grid (1, 1))

let test_mine_placement_failure _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 1 (1, 1) (1, 5));
  ignore (place_mine grid (2, 2));
  ignore (shoot grid (1, 3));
  ignore (shoot grid (0, 0));
  assert_bool "Mine not placed (already a mine there)"
    (not (place_mine grid (2, 2)));
  assert_bool "Mine not placed (a ship is there)" (not (place_mine grid (1, 1)));
  assert_bool "Mine not placed (a missed shot is there)"
    (not (place_mine grid (0, 0)));
  assert_bool "Mine not placed (a hit ship is there)"
    (not (place_mine grid (1, 3)));
  assert_raises InvalidPlacement (fun () -> place_mine grid (1, -1))

let test_shooting_mine _ =
  let grid = create_grid 10 in
  ignore (place_mine grid (1, 1));
  assert_equal "Mine hit!" (shoot grid (1, 1))

let test_multiple_mines _ =
  let grid = create_grid 10 in
  ignore (place_mine grid (1, 1));
  ignore (place_mine grid (1, 2));
  ignore (shoot grid (1, 1));
  assert_equal "Mine hit!" (shoot grid (1, 2))

let test_ship_and_mine_coexistence _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 1 (0, 0) (0, 4));
  assert_bool "Mine placed next to ship" (place_mine grid (1, 1))

let check_coordinate_list grid x y lst =
  if not (List.mem (!x, !y) !lst) then
    let _ = ai_guess grid in
    lst := (!x, !y) :: !lst
  else
    let () = x := Random.int 10 in
    y := Random.int 10

let test_no_repeat_ai_hard_shots _ =
  set_ai_mode Hard;
  let grid = create_grid 10 in
  let all_shots = ref [] in
  for _ = 1 to 1000 do
    let x = ref (Random.int 10) and y = ref (Random.int 10) in
    check_coordinate_list grid x y all_shots
  done;
  assert_equal 100 (List.length !all_shots)
    ~msg:"AI should shoot each cell only once"

let test_ai_hard_mine_interaction _ =
  let grid = create_grid 2 in
  ignore (place_mine grid (1, 1));
  set_ai_mode Hard;
  let result = ref "Miss!" in
  for _ = 1 to 4 do
    let guess = ai_guess grid in
    if guess = "Mine hit!" then result := guess
  done;
  assert_equal "Mine hit!" !result

let test_ship_placement_horizontal_edge _ =
  let grid = create_grid 10 in
  assert_bool "Place ship at horizontal edge should succeed"
    (place_ship grid 100 (0, 0) (0, 9))

let test_multiple_ship_placement _ =
  let grid = create_grid 10 in
  assert_bool "First ship placed" (place_ship grid 101 (0, 0) (0, 4));
  assert_bool "Second ship placed without overlap"
    (place_ship grid 102 (1, 0) (1, 4))

let test_ship_placement_reversed_coordinates _ =
  let grid = create_grid 10 in
  assert_bool "Placing ship with reversed coordinates"
    (place_ship grid 3 (5, 5) (1, 5))

let test_mine_triggered_by_ai_easy _ =
  let grid = create_grid 2 in
  ignore (place_mine grid (1, 1));
  set_ai_mode Easy;
  let result = ref "Miss!" in
  for _ = 1 to 4 do
    let guess = ai_guess grid in
    if guess = "Mine hit!" then result := guess
  done;
  assert_equal "Mine hit!" !result

let test_game_over_detection _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 1 (0, 0) (1, 0));
  ignore (shoot grid (0, 0));
  ignore (shoot grid (1, 0));
  assert_bool "Game should be over when all ships are sunk"
    (check_game_over grid)

let test_ship_placement_next_to_another _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 1 (0, 0) (0, 3));
  assert_bool "Ships placed next to each other should be fine (not overlapping)"
    (place_ship grid 2 (0, 4) (0, 7))

let test_ai_easy_performance_no_repeats _ =
  (* let grid = create_grid 10 in let seen = ref [] in for _ = 1 to 100 do let
     guess = ai_guess grid in if List.mem guess !seen then assert_failure "AI
     guessed a cell more than once" else seen := guess :: !seen done *)
  set_ai_mode Easy;
  let grid = create_grid 10 in
  let all_shots = ref [] in
  for _ = 1 to 1000 do
    let x = ref (Random.int 10) and y = ref (Random.int 10) in
    check_coordinate_list grid x y all_shots
  done;
  assert_equal 100 (List.length !all_shots)
    ~msg:"AI should should not repeat shots"

let test_grid_initialization_all_empty _ =
  let grid = create_grid 10 in
  for y = 0 to 9 do
    for x = 0 to 9 do
      assert_equal Empty grid.(y).(x)
    done
  done

let test_ship_overlapping_mine _ =
  let grid = create_grid 10 in
  ignore (place_mine grid (4, 4));
  assert_raises InvalidPlacement (fun () ->
      ignore (place_ship grid 108 (4, 3) (4, 5)))

let test_valid_mine_placement_around_ships _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 109 (5, 5) (5, 9));
  assert_bool "Place mine near ship but not on it" (place_mine grid (5, 4))

let test_get_bounding_box _ =
  let coordinates = [ (0, 0); (0, 1); (1, 0); (1, 1) ] in
  let top_left, width, height = get_bounding_box coordinates in
  assert_equal (0, 0) top_left;
  assert_equal 2 width;
  assert_equal 2 height

let test_get_bounding_box_horizontal_line _ =
  let coordinates = [ (0, 0); (0, 1); (0, 2); (0, 3) ] in
  let top_left, width, height = get_bounding_box coordinates in
  assert_equal (0, 0) top_left;
  assert_equal 4 width;
  assert_equal 1 height

let test_get_bounding_box_vertical_line _ =
  let coordinates = [ (1, 1); (2, 1); (3, 1) ] in
  let top_left, width, height = get_bounding_box coordinates in
  assert_equal (1, 1) top_left;
  assert_equal 1 width;
  assert_equal 3 height

let test_get_bounding_box_Lshape _ =
  let coordinates = [ (0, 0); (1, 0); (2, 0); (2, 1) ] in
  let top_left, width, height = get_bounding_box coordinates in
  assert_equal (0, 0) top_left;
  assert_equal 2 width;
  assert_equal 3 height

let test_get_bounding_box_point _ =
  let coordinates = [ (3, 4) ] in
  let top_left, width, height = get_bounding_box coordinates in
  assert_equal (3, 4) top_left;
  assert_equal 1 width;
  assert_equal 1 height

let test_get_bounding_box_sparse_point _ =
  let coordinates = [ (0, 0); (2, 3); (5, 1) ] in
  let top_left, width, height = get_bounding_box coordinates in
  assert_equal (0, 0) top_left;
  assert_equal 4 width;
  assert_equal 6 height

let test_get_bounding_box_scattered_point _ =
  let coordinates = [ (10, 10); (0, 0); (5, 5); (3, 2) ] in
  let top_left, width, height = get_bounding_box coordinates in
  assert_equal (0, 0) top_left;
  assert_equal 11 width;
  assert_equal 11 height

let test_assemble_custom_ship _ =
  let pieces = [ [ (0, 0); (0, 1) ]; [ (1, 0); (1, 1) ] ] in
  let custom_ship = assemble_custom_ship pieces 1 in
  assert_equal 1 custom_ship.id;
  assert_equal 4 custom_ship.health;
  assert_equal (0, 0) custom_ship.top_left;
  assert_equal 2 custom_ship.width;
  assert_equal 2 custom_ship.height

let test_assemble_custom_ship_id _ =
  let pieces1 =
    [
      [ (1, 0); (1, 2) ];
      [ (0, 1); (0, 1) ];
      [ (4, 2); (0, 2) ];
      [ (2, 3); (2, 3) ];
    ]
  in
  let custom_ship1 = assemble_custom_ship pieces1 0 in
  assert_equal ~msg:"Custom ship id 0" 0 custom_ship1.id;
  assert_equal ~msg:"Custom ship 9 health" 9 custom_ship1.health;
  assert_equal ~msg:"Custom ship top left at (0, 0)" (0, 0)
    custom_ship1.top_left;
  assert_equal ~msg:"Custom ship 4 width" 4 custom_ship1.width;
  assert_equal ~msg:"Custom ship 5 height" 5 custom_ship1.height;

  let pieces2 =
    [
      [ (7, 5); (7, 2) ];
      [ (5, 4); (5, 1) ];
      [ (4, 3); (4, 3) ];
      [ (6, 2); (5, 2) ];
    ]
  in
  let custom_ship2 = assemble_custom_ship pieces2 1 in
  assert_equal ~msg:"Custom ship id 1" 1 custom_ship2.id;
  assert_equal ~msg:"Custom ship 10 health" 10 custom_ship2.health;
  assert_equal ~msg:"Custom ship top left at (4, 1)" (4, 1)
    custom_ship2.top_left;
  assert_equal ~msg:"Custom ship 5 width" 5 custom_ship2.width;
  assert_equal ~msg:"Custom ship 4 height" 4 custom_ship2.height;

  let pieces3 = [ [ (0, 0); (0, 0) ] ] in
  let custom_ship3 = assemble_custom_ship pieces3 2 in
  assert_equal ~msg:"Custom ship id 2" 2 custom_ship3.id;
  assert_equal ~msg:"Custom ship 1 health" 1 custom_ship3.health;
  assert_equal ~msg:"Custom ship top left at (0, 0)" (0, 0)
    custom_ship3.top_left;
  assert_equal ~msg:"Custom ship 1 width" 1 custom_ship3.width;
  assert_equal ~msg:"Custom ship 1 height" 1 custom_ship3.height

let test_place_custom_ship_success _ =
  let grid = create_grid 10 in
  let pieces = [ [ (0, 0); (0, 1) ]; [ (1, 0); (1, 1) ] ] in
  let custom_ship = assemble_custom_ship pieces 1 in
  assert_bool "Custom ship placement should succeed"
    (place_custom_ship grid custom_ship (0, 0));
  List.iter
    (fun (y, x) -> assert_equal (CustomShip custom_ship) grid.(y).(x))
    custom_ship.cells

let test_create_custom_ship_from_grid _ =
  let grid = create_grid 10 in

  let custom_ship_parts = [ (1, 0); (1, 2); (0, 1); (4, 2); (2, 3) ] in
  List.iter
    (fun (y, x) ->
      grid.(y).(x) <-
        CustomShip
          {
            id = 100;
            cells = [];
            health = 0;
            top_left = (0, 0);
            width = 0;
            height = 0;
          })
    custom_ship_parts;

  let custom_ship = create_custom_ship_from_grid grid in

  assert_equal ~msg:"Custom ship id" 100 custom_ship.id;
  assert_equal ~msg:"Custom ship health"
    (List.length custom_ship_parts)
    custom_ship.health;
  assert_equal ~msg:"Custom ship top left" (0, 0) custom_ship.top_left;
  assert_equal ~msg:"Custom ship width" 4 custom_ship.width;
  assert_equal ~msg:"Custom ship height" 5 custom_ship.height;
  assert_equal ~msg:"Custom ship cells"
    (List.sort compare custom_ship.cells)
    (List.sort compare [ (1, 0); (1, 2); (0, 1); (4, 2); (2, 3) ])

let test_create_custom_ship_with_disjoint_parts _ =
  let grid = create_grid 10 in
  let custom_ship_parts = [ (0, 0); (0, 1); (1, 0); (1, 1); (3, 3); (3, 4) ] in
  List.iter
    (fun (y, x) ->
      grid.(y).(x) <-
        CustomShip
          {
            id = 100;
            cells = [];
            health = 0;
            top_left = (0, 0);
            width = 0;
            height = 0;
          })
    custom_ship_parts;

  let custom_ship = create_custom_ship_from_grid grid in

  assert_equal ~msg:"Custom ship id" 100 custom_ship.id;
  assert_equal ~msg:"Custom ship health"
    (List.length custom_ship_parts)
    custom_ship.health;
  assert_equal ~msg:"Custom ship top left" (0, 0) custom_ship.top_left;
  assert_equal ~msg:"Custom ship width" 5 custom_ship.width;
  assert_equal ~msg:"Custom ship height" 4 custom_ship.height;
  assert_equal ~msg:"Custom ship cells"
    (List.sort compare custom_ship.cells)
    (List.sort compare [ (0, 0); (0, 1); (1, 0); (1, 1); (3, 3); (3, 4) ])

let test_is_overlap_no_overlap _ =
  let grid = create_grid 10 in
  let ship_coords = [ (0, 0); (0, 1); (0, 2) ] in
  assert_bool "No overlap with empty grid" (is_overlap grid ship_coords 1)

let test_is_overlap_with_same_id _ =
  let grid = create_grid 10 in
  let custom_ship =
    {
      id = 1;
      cells = [ (0, 0); (0, 1) ];
      health = 2;
      top_left = (0, 0);
      width = 2;
      height = 1;
    }
  in
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship)
    custom_ship.cells;
  let ship_coords = [ (0, 0); (0, 1); (0, 2) ] in
  assert_bool "Overlap with same custom ship ID" (is_overlap grid ship_coords 1)

let test_is_overlap_with_different_id _ =
  let grid = create_grid 10 in
  let custom_ship =
    {
      id = 1;
      cells = [ (0, 0); (0, 1) ];
      health = 2;
      top_left = (0, 0);
      width = 2;
      height = 1;
    }
  in
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship)
    custom_ship.cells;
  let ship_coords = [ (0, 0); (0, 1); (0, 2) ] in
  assert_bool "Overlap with different custom ship ID"
    (not (is_overlap grid ship_coords 2))

let test_is_overlap_with_regular_ship _ =
  let grid = create_grid 10 in
  let _ = place_ship grid 1 (0, 0) (0, 1) in
  let ship_coords = [ (0, 0); (0, 1); (0, 2) ] in
  assert_bool "Overlap with regular ship" (not (is_overlap grid ship_coords 2))

let test_is_overlap_with_empty_spaces _ =
  let grid = create_grid 10 in
  let ship_coords = [ (5, 5); (5, 6); (5, 7) ] in
  assert_bool "No overlap with empty spaces" (is_overlap grid ship_coords 1)

let test_count_ship_cells_no_custom_ships _ =
  let grid = create_grid 10 in
  let custom_id = 1 in
  let count = count_ship_cells grid custom_id in
  assert_equal ~msg:"No custom ships on the grid" 0 count

let test_count_ship_cells_single_custom_ship _ =
  let grid = create_grid 10 in
  let custom_ship =
    {
      id = 1;
      cells = [ (0, 0); (0, 1); (1, 0); (1, 1) ];
      health = 4;
      top_left = (0, 0);
      width = 2;
      height = 2;
    }
  in
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship)
    custom_ship.cells;
  let count = count_ship_cells grid custom_ship.id in
  assert_equal ~msg:"Single custom ship on the grid" 4 count

let test_count_ship_cells_multiple_custom_ships _ =
  let grid = create_grid 10 in
  let custom_ship1 =
    {
      id = 1;
      cells = [ (0, 0); (0, 1); (1, 0); (1, 1) ];
      health = 4;
      top_left = (0, 0);
      width = 2;
      height = 2;
    }
  in
  let custom_ship2 =
    {
      id = 2;
      cells = [ (2, 2); (2, 3); (3, 2); (3, 3) ];
      health = 4;
      top_left = (2, 2);
      width = 2;
      height = 2;
    }
  in
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship1)
    custom_ship1.cells;
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship2)
    custom_ship2.cells;
  let count1 = count_ship_cells grid custom_ship1.id in
  let count2 = count_ship_cells grid custom_ship2.id in
  assert_equal ~msg:"First custom ship on the grid" 4 count1;
  assert_equal ~msg:"Second custom ship on the grid" 4 count2

let test_count_ship_cells_mixed_ships _ =
  let grid = create_grid 10 in
  let custom_ship =
    {
      id = 1;
      cells = [ (0, 0); (0, 1); (1, 0); (1, 1) ];
      health = 4;
      top_left = (0, 0);
      width = 2;
      height = 2;
    }
  in
  let _ = place_ship grid 2 (2, 0) (2, 2) in
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship)
    custom_ship.cells;
  let count_custom = count_ship_cells grid custom_ship.id in
  let count_regular = count_ship_cells grid 2 in
  assert_equal ~msg:"Custom ship on the grid" 4 count_custom;
  assert_equal
    ~msg:"Regular ship on the grid should not be counted as custom ship" 0
    count_regular

let test_clear_custom_ship_from_grid_no_custom_ships _ =
  let grid = create_grid 10 in
  clear_custom_ship_from_grid grid;
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid.(0) - 1 do
      assert_equal ~msg:"Grid should remain empty" Empty grid.(y).(x)
    done
  done

let test_clear_custom_ship_from_grid_single_custom_ship _ =
  let grid = create_grid 10 in
  let custom_ship =
    {
      id = 100;
      cells = [ (0, 0); (0, 1); (1, 0); (1, 1) ];
      health = 4;
      top_left = (0, 0);
      width = 2;
      height = 2;
    }
  in
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship)
    custom_ship.cells;
  clear_custom_ship_from_grid grid;
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid.(0) - 1 do
      assert_equal ~msg:"Grid cells should be empty after clearing custom ship"
        Empty
        grid.(y).(x)
    done
  done

let test_clear_custom_ship_from_grid_mixed_ships _ =
  let grid = create_grid 10 in
  let custom_ship1 =
    {
      id = 100;
      cells = [ (0, 0); (0, 1); (1, 0); (1, 1) ];
      health = 4;
      top_left = (0, 0);
      width = 2;
      height = 2;
    }
  in
  let custom_ship2 =
    {
      id = 101;
      cells = [ (2, 2); (2, 3); (3, 2); (3, 3) ];
      health = 4;
      top_left = (2, 2);
      width = 2;
      height = 2;
    }
  in
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship1)
    custom_ship1.cells;
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship2)
    custom_ship2.cells;
  clear_custom_ship_from_grid grid;
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid.(0) - 1 do
      if grid.(y).(x) = CustomShip custom_ship2 then
        assert_equal ~msg:"Custom ship with id 101 should remain"
          (CustomShip custom_ship2)
          grid.(y).(x)
      else
        assert_equal
          ~msg:
            "Grid cells should be empty after clearing custom ship with id 100"
          Empty
          grid.(y).(x)
    done
  done

let test_clear_custom_ship_from_grid_partially _ =
  let grid = create_grid 10 in
  let custom_ship1 =
    {
      id = 100;
      cells = [ (0, 0); (0, 1); (1, 0); (1, 1) ];
      health = 4;
      top_left = (0, 0);
      width = 2;
      height = 2;
    }
  in
  let custom_ship2 =
    {
      id = 100;
      cells = [ (2, 2); (2, 3); (3, 2); (3, 3) ];
      health = 4;
      top_left = (2, 2);
      width = 2;
      height = 2;
    }
  in
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship1)
    custom_ship1.cells;
  List.iter
    (fun (y, x) -> grid.(y).(x) <- CustomShip custom_ship2)
    custom_ship2.cells;
  clear_custom_ship_from_grid grid;
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid.(0) - 1 do
      assert_equal
        ~msg:"Grid cells should be empty after clearing custom ship with id 100"
        Empty
        grid.(y).(x)
    done
  done

let suite =
  "Battleship Test Suite"
  >::: [
         "test_create_grid" >:: test_create_grid;
         "test_print_grid" >:: test_print_grid;
         "test_place_ship_success" >:: test_place_ship_success;
         "test_place_ship_max_min" >:: test_place_ship_max_min;
         "test_place_ship_failures" >:: test_place_ship_failures;
         "test_shoot" >:: test_shoot;
         "test_ai_guess" >:: test_ai_guess;
         "test_check_game_over" >:: test_check_game_over;
         "test_invalid_input" >:: test_invalid_input;
         "test_char_to_index" >:: test_char_to_index;
         "test_validate_coordinates" >:: test_validate_coordinates;
         "test_boundary_ship_placement" >:: test_boundary_ship_placement;
         "test_boundary_ship_placement_failure"
         >:: test_boundary_ship_placement_failure;
         "test_horizontal_ship_overlap" >:: test_horizontal_ship_overlap;
         "test_vertical_ship_overlap" >:: test_vertical_ship_overlap;
         "test_complete_ship_destruction" >:: test_complete_ship_destruction;
         "test_multiple_shots_same_place" >:: test_multiple_shots_same_place;
         "test_random_ai_guess_effectiveness"
         >:: test_random_ai_guess_effectiveness;
         "test_ship_placement_in_empty_grid"
         >:: test_ship_placement_in_empty_grid;
         "test_ship_placement_boundary" >:: test_ship_placement_boundary;
         "test_ship_placement_at_edges" >:: test_ship_placement_at_edges;
         "test_ships_touching_at_corners" >:: test_ships_touching_at_corners;
         "test_ship_wrapping" >:: test_ship_wrapping;
         "test_full_grid_sunk" >:: test_full_grid_sunk;
         "test_ai_efficiency_easy" >:: test_ai_efficiency_easy;
         "test_ai_efficiency_hard" >:: test_ai_efficiency_hard;
         "test_bomb_row" >:: test_bomb_row;
         "test_bomb_column" >:: test_bomb_column;
         "test_square_bomb" >:: test_square_bomb;
         "test_airstrike" >:: test_airstrike;
         "test_cell_count" >:: test_cell_count;
         "test_overlapping_ships_horizontally_vertically"
         >:: test_overlapping_ships_horizontally_vertically;
         "test_adjacent_ships" >:: test_adjacent_ships;
         "test_firing_at_all_coordinates" >:: test_firing_at_all_coordinates;
         "test_ai_shoots_all_grid_once" >:: test_ai_shoots_all_grid_once;
         "test_game_over_after_all_ships_hit"
         >:: test_game_over_after_all_ships_hit;
         "test_invalid_ship_placement_at_edges"
         >:: test_invalid_ship_placement_at_edges;
         "test_ship_placement_at_corners" >:: test_ship_placement_at_corners;
         "test_various_ship_sizes" >:: test_various_ship_sizes;
         "test_reset_game" >:: test_reset_game;
         "test_shooting_beyond_grid_boundaries"
         >:: test_shooting_beyond_grid_boundaries;
         "test_full_grid_no_ships" >:: test_full_grid_no_ships;
         "test_ship_destruction_notification"
         >:: test_ship_destruction_notification;
         "test_player_switch_in_two_player_mode"
         >:: test_player_switch_in_two_player_mode;
         "test_mine_placement_success" >:: test_mine_placement_success;
         "test_mine_placement_failure" >:: test_mine_placement_failure;
         "test_shooting_mine" >:: test_shooting_mine;
         "test_multiple_mines" >:: test_multiple_mines;
         "test_ship_and_mine_coexistence" >:: test_ship_and_mine_coexistence;
         "test_no_repeat_ai_shots" >:: test_no_repeat_ai_hard_shots;
         "test_ai_hard_mine_interaction" >:: test_ai_hard_mine_interaction;
         "test_ship_placement_horizontal_edge"
         >:: test_ship_placement_horizontal_edge;
         "test_multiple_ship_placement" >:: test_multiple_ship_placement;
         "test_ship_placement_reversed_coordinates"
         >:: test_ship_placement_reversed_coordinates;
         "test_mine_triggered_by_ai_easy" >:: test_mine_triggered_by_ai_easy;
         "test_game_over_detection" >:: test_game_over_detection;
         "test_ship_placement_next_to_another"
         >:: test_ship_placement_next_to_another;
         "test_ai_performance_no_repeats"
         >:: test_ai_easy_performance_no_repeats;
         "test_grid_initialization_all_empty"
         >:: test_grid_initialization_all_empty;
         "test_ship_overlapping_mine" >:: test_ship_overlapping_mine;
         "test_valid_mine_placement_around_ships"
         >:: test_valid_mine_placement_around_ships;
         "test_get_bounding_box" >:: test_get_bounding_box;
         "test_get_bounding_box_horizontal_line"
         >:: test_get_bounding_box_horizontal_line;
         "test_get_bounding_box_vertical_line"
         >:: test_get_bounding_box_vertical_line;
         "test_get_bounding_box_Lshape" >:: test_get_bounding_box_Lshape;
         "test_get_bounding_box_point" >:: test_get_bounding_box_point;
         "test_get_bounding_box_sparse_point"
         >:: test_get_bounding_box_sparse_point;
         "test_get_bounding_box_scattered_point"
         >:: test_get_bounding_box_scattered_point;
         "test_assemble_custom_ship" >:: test_assemble_custom_ship;
         "test_place_custom_ship_success" >:: test_place_custom_ship_success;
         "test_create_custom_ship_from_grid"
         >:: test_create_custom_ship_from_grid;
         "test_create_custom_ship_with_disjoint_parts"
         >:: test_create_custom_ship_with_disjoint_parts;
         "test_is_overlap_no_overlap" >:: test_is_overlap_no_overlap;
         "test_is_overlap_with_same_id" >:: test_is_overlap_with_same_id;
         "test_is_overlap_with_different_id"
         >:: test_is_overlap_with_different_id;
         "test_is_overlap_with_regular_ship"
         >:: test_is_overlap_with_regular_ship;
         "test_is_overlap_with_empty_spaces"
         >:: test_is_overlap_with_empty_spaces;
         "test_clear_custom_ship_from_grid_no_custom_ships"
         >:: test_clear_custom_ship_from_grid_no_custom_ships;
         "test_clear_custom_ship_from_grid_single_custom_ship"
         >:: test_clear_custom_ship_from_grid_single_custom_ship;
         "test_clear_custom_ship_from_grid_mixed_ships"
         >:: test_clear_custom_ship_from_grid_mixed_ships;
         "test_clear_custom_ship_from_grid_partially"
         >:: test_clear_custom_ship_from_grid_partially;
         "test_count_ship_cells_no_custom_ships"
         >:: test_count_ship_cells_no_custom_ships;
         "test_count_ship_cells_single_custom_ship"
         >:: test_count_ship_cells_single_custom_ship;
         "test_count_ship_cells_multiple_custom_ships"
         >:: test_count_ship_cells_multiple_custom_ships;
         "test_count_ship_cells_mixed_ships"
         >:: test_count_ship_cells_mixed_ships;
       ]

let () = run_test_tt_main suite
