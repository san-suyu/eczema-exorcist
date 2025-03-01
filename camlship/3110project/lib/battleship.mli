(** custom ship structure with detailed properties of each ship *)

type custom_ship = {
  id : int;
  (* unique identifier for a ship *)
  cells : (int * int) list;
  (* list of coordinates occupied by ship *)
  health : int;
  (* health of the ship, typically equal to number of cells it occupies on
     grid *)
  top_left : int * int;
  (* top-left coordinate of the ship boundary*)
  width : int;
  (* width of the ships boundary *)
  height : int; (* height of ships boundary *)
}

(* Abstraction function: A custom_ship represents a unique ship on the grid with
   [id] as its identifier. The ship occupies [cells], each corresponding to a
   coordinate on the grid. [health] indicates the number of unhit cells, with
   initial value equal to the length of [cells]. [top_left] is the smallest
   coordinate by value that defines the upper left corner of the ship's bounding
   box. [width] and [height] define the dimensions of the smallest rectangle
   that can enclose the entire ship.

   Representation invariant: - [health] must be non-negative. - All coordinates
   in [cells] must be unique and within the grid bounds determined by the grid
   size. - [width] and [height] should be correctly calculated to contain all
   coordinates in [cells]. - [top_left] must actually be the coordinate with the
   smallest x and y values in [cells]. *)

type cell =
  (* types of cells that can be present on the game grid *)
  | Empty
  (* empty cell with no ships, mines, hits, misses, etc *)
  | Ship of int
  (* cell containing part of a ship identified by an int *)
  | Mine
  (* cell containing a mine *)
  | Hit of int
  (* cell that was hit containing part of a ship identified by an int *)
  | Exploded
  (* cell where a mine has exploded *)
  | Miss
  (* cell that was shot at but was empty *)
  | CustomShip of custom_ship
  (* cell occupied by a custom ship *)
  | HitCustom of custom_ship
(* cell containing a hit part of a custom ship *)

type grid = cell array array
(** type representing the game grid as a 2D array of cells *)

exception InvalidPlacement
(** exceptions that can be raised during the game. Raised when a ship or mine
    cannot be placed at the specified location *)

exception InvalidInput
(** raised when an input, such as coordinates, are invalid *)

val create_grid : int -> grid
(** [create_grid size] creates grid of specified [size] initialized with Empty
    cells *)

val print_grid : grid -> bool -> string -> unit
(** [print_grid grid show name] prints current state of the grid to stdout. If
    [show] is true, the ships on the grid will be shown. *)

val place_ship : grid -> int -> int * int -> int * int -> bool
(** [place_ship grid num (y1, x1) (y2, x2)] tries to place a ship at specified
    coordinates, filling the cells including [(y1, x1)] and [(y1, x1)] on
    [grid], returns true if successful *)

val place_mine : grid -> int * int -> bool
(** [place_mine grid (y, x)] tries to place a mine at specified coordinates
    [(y, x)] on [grid], returns true if successful *)

val shoot : grid -> int * int -> string
(** [shoot grid (y, x)] processes a shot at the specified coordinates [(y, x)],
    returning result message *)

val mine_shot : grid -> string
(** [mine_shot grid] handles explosion of a mine on [grid], returning a result
    message *)

val ai_guess : grid -> string
(** [ai_guess grid] makes a guess for the AI based on the set AI mode, returning
    result message *)

val check_game_over : grid -> bool
(** [check_game_over grid] checks if game is over for [grid] (all ships have
    been sunk) *)

val char_to_index : char -> int
(** [char_to_index character] converts [character] to the corresponding index
    (e.g., 'A' to 0) *)

val validate_coordinates : int -> int -> int -> bool
(** [validate_coordinates x y size] validates if specified coordinates [x] and
    [y] are within the grid boundaries, [size] *)

val validate_bomb : int -> int -> int -> bool
(** [validate_bomb x y size] validates if a bomb placement coordinates [x] and
    [y] are within the grid boundaries, [size] *)

val random_place_ships : grid -> unit
(** [random_place_ships grid] randomly places ships on [grid] *)

val random_place_mines : grid -> int -> unit
(** [random_place_mines grid num] randomly places a specified number [num] of
    mines on [grid] *)

type ai_mode =
  (* AI difficulty modes *)
  | Easy
  (* easy difficulty setting for AI *)
  | Hard
(* hard difficulty setting for AI *)

val set_ai_mode : ai_mode -> unit
(** [set_ai_mode mode] sets current AI difficulty mode to [mode] *)

val get_ai_mode : unit -> ai_mode
(** [get_ai_mode ()] retrieves current AI difficulty mode *)

val count_cell_type : grid -> cell -> int
(** [count_cell_type grid cell_type] counts cells of a specified type
    [cell_type] on [grid] *)

val count_hit_cells : grid -> int
(** [count_hit_cells grid] counts number of hit cells on [grid] *)

val assemble_custom_ship : (int * int) list list -> int -> custom_ship
(** [assemble_custom_ship pieces id] assembles a custom ship from a list of
    coordinates [pieces] *)

val is_overlap : grid -> (int * int) list -> int -> bool

val place_custom_ship : grid -> custom_ship -> int * int -> bool
(** [place_custom_ship grid custom_ship (y, x)] attempts to place [custom_ship]
    on [grid] at specified coordinates [(y, x)], returns true if successful *)

val get_ship_health_length : unit -> int
(** [get_ship_health_length ()] returns the health and length of all ships
    combined *)

val get_bounding_box : (int * int) list -> (int * int) * int * int
(** [get_bounding_box coordinates] calculates bounding box for a list of
    coordinates [coordinates] *)

val create_custom_ship_from_grid : grid -> custom_ship
(** [create_custom_ship_from_grid grid] creates a custom ship from existing
    cells on [grid] *)

val print_custom_ship : custom_ship -> unit
(** [print_custom_ship custom_ship] prints details of [custom_ship] *)

val count_ship_cells : grid -> int -> int
val clear_custom_ship_from_grid : grid -> unit
val parse_coord : string -> int * int
val get_adjacent_coords : int * int -> (int * int) list

val is_adjacent_to_existing :
  grid -> (int * int) list -> int -> bool * (int * int) list

val get_id : cell -> int option
