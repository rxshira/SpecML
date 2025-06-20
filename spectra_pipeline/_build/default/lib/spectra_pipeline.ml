(*define the metadata type with all essential fields*)
type label_metadata = {
  filename : string;
  lines : int;
  line_samples : int;
  bands : int;
  sample_type : string;
  sample_bits : int;
  core_base : float;
  core_multiplier : float;
  data_filename : string;
  data_offset : int;
  band_bin_center : float array;  (* new spectral axis field *)
}

(*location of qub and lbl files on my device*)

let data_dir = "/Users/mjulia/Documents/specMLData/saturn/mainMission/season1"

(*reading files*)

let list_label_files () : string list =
  Sys.readdir data_dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".lbl")
  |> List.map (Filename.concat data_dir)

(*.lbl parser*)

let rec parse_lbl ic acc =
  match input_line ic with
  | line ->
    let line = String.trim line in
    if String.contains line '=' then
      let parts = String.split_on_char '=' line in
      (match parts with
      | [key; value] ->
        let key = String.trim key in
        let value = String.trim value in
        parse_lbl ic ((key, value) :: acc)
      | _ -> parse_lbl ic acc)
    else
      parse_lbl ic acc
  | exception End_of_file -> List.rev acc

(* Parses a single .lbl file given its filename *)
let parse_lbl_file (filename : string) : (string * string) list =
  let ic = open_in filename in
  let result = parse_lbl ic [] in
  close_in ic;
  result

(* Parses all .lbl files *)
let parse_all_labels () : (string * (string * string) list) list =
  let files = list_label_files () in
  List.map (fun f -> (f, parse_lbl_file f)) files

(* Parses string "(x,y,z)" into tuple *)
let parse_core_items s =
  try Scanf.sscanf s "(%d,%d,%d)" (fun a b c -> (a,b,c))
  with _ -> failwith ("Malformed CORE_ITEMS: " ^ s)

(* Parses BAND_BIN_CENTER string into float array *)
let parse_float_array str =
  let cleaned =
    str
    |> String.trim
    |> fun s ->
      if String.length s >= 2 && s.[0] = '(' && s.[String.length s - 1] = ')'
      then String.sub s 1 (String.length s - 2)
      else s
  in
  cleaned
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map float_of_string
  |> Array.of_list

(* Extracts metadata from key-value pairs *)
let extract_metadata (file : string) (pairs : (string * string) list) : label_metadata option =
  let get key = List.assoc_opt key pairs |> Option.value ~default:"" |> String.trim in
  let core_items_raw = get "CORE_ITEMS" in

  if core_items_raw = "" then (
    Printf.eprintf "Skipping %s: missing CORE_ITEMS\n" file;
    None
  ) else
    try
      let sample, band, line = parse_core_items core_items_raw in
      let band_bin_center =
        try parse_float_array (get "BAND_BIN_CENTER")
        with _ -> [||]
      in
      Some {
        filename = Filename.basename file;
        lines = line;
        line_samples = sample;
        bands = band;
        sample_type = get "CORE_ITEM_TYPE";
        sample_bits = int_of_string (get "CORE_ITEM_BYTES") * 8;
        core_base = float_of_string (get "CORE_BASE");
        core_multiplier = float_of_string (get "CORE_MULTIPLIER");
        data_filename = get "^QUBE";
        data_offset = 0; (* You can parse if needed *)
        band_bin_center;
      }
    with _ ->
      Printf.eprintf "Skipping %s due to malformed CORE_ITEMS: %s\n" file core_items_raw;
      None



(*


let parse_lbl_file filepath =
  let ic = open_in filepath in
  let result = parse_lbl ic [] in
  close_in ic;
  result

let parse_all_labels () : (string * (string * string) list) list =
  list_label_files ()
  |> List.map (fun filepath ->
      let parsed = parse_lbl_file filepath in
      (filepath, parsed))

(*helper to parse a string "(1,2,3)" into a tuple of ints*)
let parse_core_items s =
  try Scanf.sscanf s "(%d,%d,%d)" (fun a b c -> (a,b,c))
  with _ -> failwith ("Malformed CORE_ITEMS: " ^ s)

(*helper to parse BAND_BIN_CENTER as float array*)
let parse_float_array str =
  let cleaned =
    str
    |> String.trim
    |> fun s ->
      if String.length s >= 2 && s.[0] = '(' && s.[String.length s - 1] = ')'
      then String.sub s 1 (String.length s - 2)
      else s
  in
  cleaned
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.map float_of_string
  |> Array.of_list

(*extract metadata record from pairs*)
let extract_metadata (file : string) (pairs : (string * string) list) : label_metadata =
  let get key = List.assoc_opt key pairs |> Option.value ~default:"" |> String.trim in
  let get_int key = int_of_string (get key) in
  let get_float key = float_of_string (get key) in

  let filename = Filename.basename file in
  let sample, band, line = parse_core_items (get "CORE_ITEMS") in

  let data_filename, data_offset =
    try
      Scanf.sscanf (get "^QUBE") "(\"%s\", %d)" (fun f o -> (f, o))
    with _ ->
      failwith ("Malformed ^QUBE field in " ^ filename)
  in

  let band_bin_center =
    try parse_float_array (get "BAND_BIN_CENTER")
    with _ -> [||]
  in

  {
    filename;
    lines = line;
    line_samples = sample;
    bands = band;
    sample_type = get "CORE_ITEM_TYPE";
    sample_bits = get_int "CORE_ITEM_BYTES" * 8;
    core_base = get_float "CORE_BASE";
    core_multiplier = get_float "CORE_MULTIPLIER";
    data_filename;
    data_offset;
    band_bin_center;
  }

*)
