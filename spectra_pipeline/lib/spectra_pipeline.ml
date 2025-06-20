(*

(*define the metadata type with all essential fields*)

(*ALL type definitions are always in the begining of the file*)

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

(************)


let skipped_files = ref 0

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
    skipped_files := !skipped_files + 1;
    None
  ) else
    try
      let sample, band, line = parse_core_items core_items_raw in
      let band_bin_center =
        let raw = get "BAND_BIN_CENTER" in
        Printf.printf "DEBUG [%s] BAND_BIN_CENTER raw: %s\n" file raw;
        try parse_float_array raw
        with _ ->
          Printf.printf "❌ Failed to parse BAND_BIN_CENTER for %s\n" file;
          [||]
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
        data_offset = 0;
        band_bin_center;
      }
    with _ ->
      Printf.eprintf "Skipping %s due to malformed CORE_ITEMS: %s\n" file core_items_raw;
      skipped_files := !skipped_files + 1;
      None

(*
let extract_metadata (file : string) (pairs : (string * string) list) : label_metadata option =
  let get key = List.assoc_opt key pairs |> Option.value ~default:"" |> String.trim in
  let core_items_raw = get "CORE_ITEMS" in

  if core_items_raw = "" then (
    Printf.eprintf "Skipping %s: missing CORE_ITEMS\n" file;
    skipped_files := !skipped_files + 1;
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
      skipped_files := !skipped_files + 1;
      None
*)

(***************)

(* CSV export *)

(* Element detection based on spectral ranges *)

let detect_elements (bands : float array) : string list =
  Printf.printf "DEBUG detect_elements: bands length = %d\n" (Array.length bands);
  let has_range low high =
    Array.exists (fun b -> b >= low && b <= high) bands
  in
  let table = [
    ("Water (H₂O)", (1.4, 1.5));
    ("Methane (CH₄)", (2.2, 2.4));
    ("Carbon Dioxide (CO₂)", (1.9, 2.1));
    ("Ammonia (NH₃)", (2.0, 2.3));
    ("Hydrogen Sulfide (H₂S)", (3.9, 4.1));
    ("Sulfur Dioxide (SO₂)", (4.0, 4.2));
  ] in
  List.fold_left (fun acc (name, (low, high)) ->
    if has_range low high then (
      Printf.printf "Element detected: %s in range %.2f-%.2f\n" name low high;
      name :: acc
    ) else acc
  ) [] table


(* Write metadata to CSV file *)
let write_csv (metadata : label_metadata list) (path : string) =
  let oc = open_out path in
  Printf.fprintf oc "Filename,Lines,Line Samples,Bands,First Band (µm),Last Band (µm),Possible Elements\n";
  List.iter (fun m ->
    let first_band = if Array.length m.band_bin_center > 0 then string_of_float m.band_bin_center.(0) else "" in
    let last_band = if Array.length m.band_bin_center > 0 then string_of_float m.band_bin_center.(Array.length m.band_bin_center - 1) else "" in
    let elements =
        let () =
            Printf.printf "DEBUG: running detect_elements for %s with %d bands\n"
            m.filename (Array.length m.band_bin_center)
        in
        detect_elements m.band_bin_center |> String.concat ";"
    in

    Printf.fprintf oc "%s,%d,%d,%d,%s,%s,%s\n"
      m.filename m.lines m.line_samples m.bands first_band last_band elements
  ) metadata;
  close_out oc
(*
let run_export () =
  let parsed = parse_all_labels () in
  let valid = List.filter_map (fun (file, kvs) -> extract_metadata file kvs) parsed in
  write_csv valid "spectral_metadata.csv";
  Printf.printf "✔ Exported %d files to spectral_metadata.csv\n" (List.length valid);
  Printf.printf "❌ Skipped %d files\n" !skipped_files
*)

let run_export () =
  Printf.printf "🚨 run_export started 🚨\n";
  try
    let parsed = parse_all_labels () in
    let valid = List.filter_map (fun (file, kvs) -> extract_metadata file kvs) parsed in
    Printf.printf "Parsed %d files, valid metadata: %d\n" (List.length parsed) (List.length valid);
    write_csv valid "spectral_metadata.csv";
    Printf.printf "✔ Exported %d files to spectral_metadata.csv\n" (List.length valid);
    Printf.printf "❌ Skipped %d files\n" !skipped_files
  with e ->
    Printf.eprintf "Exception during export: %s\n" (Printexc.to_string e)

*)


(****************************************************)
(*define the metadata type with all essential fields*)

(*ALL type definitions are always in the begining of the file*)

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

(************)

let skipped_files = ref 0

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
  |> List.filter (fun s -> s <> "")
  |> List.filter_map (fun s -> try Some (float_of_string s) with _ -> None)
  |> Array.of_list

(* Extracts metadata from key-value pairs *)
let extract_metadata (file : string) (pairs : (string * string) list) : label_metadata option =
  let get key =
    match List.find_opt (fun (k, _) -> String.uppercase_ascii (String.trim k) = String.uppercase_ascii key) pairs with
    | Some (_, v) -> String.trim v
    | None -> ""
  in
  let core_items_raw = get "CORE_ITEMS" in

  if core_items_raw = "" then (
    Printf.eprintf "Skipping %s: missing CORE_ITEMS\n" file;
    skipped_files := !skipped_files + 1;
    None
  ) else
    try
      let sample, band, line = parse_core_items core_items_raw in
      let band_bin_center =
        let raw = get "BAND_BIN_CENTER" in
        Printf.printf "DEBUG [%s] BAND_BIN_CENTER raw: %s\n" file raw;
        try parse_float_array raw
        with _ ->
          Printf.printf "❌ Failed to parse BAND_BIN_CENTER for %s\n" file;
          [||]
      in
      if Array.length band_bin_center = 0 then (
        Printf.printf "❌ No usable BAND_BIN_CENTER for %s\n" file;
      );
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
        data_offset = 0;
        band_bin_center;
      }
    with _ ->
      Printf.eprintf "Skipping %s due to malformed CORE_ITEMS: %s\n" file core_items_raw;
      skipped_files := !skipped_files + 1;
      None

(***************)

(* CSV export *)

(* Element detection based on spectral ranges *)

let detect_elements (bands : float array) : string list =
  Printf.printf "DEBUG detect_elements: bands length = %d\n" (Array.length bands);
  let has_range low high =
    Array.exists (fun b -> b >= low && b <= high) bands
  in
  let table = [
    ("Water (H₂O)", (1.4, 1.5));
    ("Methane (CH₄)", (2.2, 2.4));
    ("Carbon Dioxide (CO₂)", (1.9, 2.1));
    ("Ammonia (NH₃)", (2.0, 2.3));
    ("Hydrogen Sulfide (H₂S)", (3.9, 4.1));
    ("Sulfur Dioxide (SO₂)", (4.0, 4.2));
  ] in
  List.fold_left (fun acc (name, (low, high)) ->
    if has_range low high then (
      Printf.printf "Element detected: %s in range %.2f-%.2f\n" name low high;
      name :: acc
    ) else acc
  ) [] table


(* Write metadata to CSV file *)
let write_csv (metadata : label_metadata list) (path : string) =
  let oc = open_out path in
  Printf.fprintf oc "Filename,Lines,Line Samples,Bands,First Band (µm),Last Band (µm),Possible Elements\n";
  List.iter (fun m ->
    let first_band = if Array.length m.band_bin_center > 0 then string_of_float m.band_bin_center.(0) else "" in
    let last_band = if Array.length m.band_bin_center > 0 then string_of_float m.band_bin_center.(Array.length m.band_bin_center - 1) else "" in
    let elements =
        let () =
            Printf.printf "DEBUG: running detect_elements for %s with %d bands\n"
            m.filename (Array.length m.band_bin_center)
        in
        detect_elements m.band_bin_center |> String.concat ";"
    in

    Printf.fprintf oc "%s,%d,%d,%d,%s,%s,%s\n"
      m.filename m.lines m.line_samples m.bands first_band last_band elements
  ) metadata;
  close_out oc

let run_export () =
  Printf.printf "🚨 run_export started 🚨\n";
  try
    let parsed = parse_all_labels () in
    let valid = List.filter_map (fun (file, kvs) -> extract_metadata file kvs) parsed in
    Printf.printf "Parsed %d files, valid metadata: %d\n" (List.length parsed) (List.length valid);
    write_csv valid "spectral_metadata.csv";
    Printf.printf "✔ Exported %d files to spectral_metadata.csv\n" (List.length valid);
    Printf.printf "❌ Skipped %d files\n" !skipped_files
  with e ->
    Printf.eprintf "Exception during export: %s\n" (Printexc.to_string e)

