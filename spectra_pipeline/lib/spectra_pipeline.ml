(* SPECML hyperspectral analysis - Shira Rubin 2025 *)

(* SPECML hyperspectral analysis - purely functional *)

type element = H2O | CH4 | CO2 | NH3 | H2S | SO2 | Unknown of string

type sample = {
  filename : string;
  lines : int option;
  line_samples : int option;
  bands : int option;
  sample_type : string option;
  sample_bits : int option;
  core_base : float option;
  core_multiplier : float option;
  data_filename : string option;
  data_offset : int option;
  band_bin_center : float array option;
}

type classification = {
  sample : sample;
  elements : element list;
  confidence : float option;
}

let data_dir = "/Users/mjulia/Documents/specMLData/saturn/finale"

(* pure safe parsing *)
let safe_int s =
  try Some (int_of_string (String.trim s)) with _ -> None

let safe_float s =
  try Some (float_of_string (String.trim s)) with _ -> None

(* pure recursive band parsing *)
let parse_bands str =
  let clean_parens s =
    if String.length s >= 2 && s.[0] = '(' && s.[String.length s - 1] = ')' 
    then String.sub s 1 (String.length s - 2)
    else s in
  
  let rec parse_float_list = function
    | [] -> []
    | h :: t -> 
      match safe_float h with
      | Some f -> f :: parse_float_list t
      | None -> parse_float_list t in
  
  try
    let cleaned = clean_parens (String.trim str) in
    let parts = String.split_on_char ',' cleaned 
                |> List.map String.trim 
                |> List.filter ((<>) "") in
    let floats = parse_float_list parts |> Array.of_list in
    if Array.length floats > 0 then Some floats else None
  with _ -> None

let parse_core_items s =
  try 
    Scanf.sscanf s "(%d,%d,%d)" (fun a b c -> Some (a, b, c))
  with _ -> None

let get_lbl_files () =
  let all_files = Sys.readdir data_dir |> Array.to_list in
  Printf.printf "all files count: %d\n" (List.length all_files);
  let lbl_files = List.filter (fun f ->
    let result = Filename.check_suffix f ".lbl" || Filename.check_suffix f ".qub.lbl" in
    if result then Printf.printf "matched: %s\n" f;
    result
  ) all_files in
  Printf.printf "filtered lbl files: %d\n" (List.length lbl_files);
  (* ADD THIS LINE - map to full paths *)
  List.map (Filename.concat data_dir) lbl_files

(* pure recursive lbl parsing *)
let parse_lbl_file filename =
  let rec read_lines ic acc =
    match input_line ic with
    | line ->
      let line = String.trim line in
      if String.contains line '=' then
        match String.split_on_char '=' line with
        | [key; value] -> read_lines ic ((String.trim key, String.trim value) :: acc)
        | _ -> read_lines ic acc
      else read_lines ic acc
    | exception End_of_file -> List.rev acc in
  
  try
    let ic = open_in filename in
    let result = read_lines ic [] in
    close_in ic;
    Some result
  with _ -> None

(* pure recursive key lookup *)
let rec find_key key = function
  | [] -> None
  | (k, v) :: t -> 
    if String.uppercase_ascii k = String.uppercase_ascii key 
    then Some v 
    else find_key key t

(* pure metadata extraction *)
let extract_metadata filename pairs =
  let get = find_key in
  match get "CORE_ITEMS" pairs with
  | None -> None
  | Some core_str ->
    match parse_core_items core_str with
    | None -> None  
    | Some (line_samples, bands, lines) ->
      Some {
        filename = Filename.basename filename;
        lines = Some lines;
        line_samples = Some line_samples; 
        bands = Some bands;
        sample_type = get "CORE_ITEM_TYPE" pairs;
        sample_bits = (match get "CORE_ITEM_BYTES" pairs with
               | Some s -> safe_int s |> Option.map (( * ) 8)
               | None -> None);
        core_base = Option.bind (get "CORE_BASE" pairs) safe_float;
        core_multiplier = Option.bind (get "CORE_MULTIPLIER" pairs) safe_float;
        data_filename = get "^QUBE" pairs;
        data_offset = Some 0;
        band_bin_center = Option.bind (get "BAND_BIN_CENTER" pairs) parse_bands;
      }

(* pure recursive sample processing *)
let rec process_files = function
  | [] -> []
  | filename :: rest ->
    let result = match parse_lbl_file filename with
      | None -> None
      | Some pairs -> extract_metadata filename pairs in
    match result with
    | None -> process_files rest
    | Some sample -> sample :: process_files rest

(* pure sample filtering *)
let rec filter_valid = function
  | [] -> []
  | sample :: rest ->
    let is_valid = match sample.band_bin_center with 
      | Some bands -> Array.length bands > 0
      | None -> false in
    if is_valid 
    then sample :: filter_valid rest
    else filter_valid rest

(* pure element detection *)
let detect_elements bands_opt =
  let has_range bands low hi = 
    let rec check_bands i =
      if i >= Array.length bands then false
      else if bands.(i) >= low && bands.(i) <= hi then true
      else check_bands (i + 1) in
    check_bands 0 in
  
  match bands_opt with
  | None -> []
  | Some bands ->
    let checks = [
      (H2O, (1.4, 1.5)); (CH4, (2.2, 2.4)); (CO2, (1.9, 2.1));
      (NH3, (2.0, 2.3)); (H2S, (3.9, 4.1)); (SO2, (4.0, 4.2));
    ] in
    let rec check_elements acc = function
      | [] -> acc
      | (el, (low, hi)) :: rest ->
        if has_range bands low hi 
        then check_elements (el :: acc) rest
        else check_elements acc rest in
    check_elements [] checks

(* pure classification *)
let classify_sample sample =
  let elements = detect_elements sample.band_bin_center in
  let confidence = if List.length elements > 0 then Some 0.8 else Some 0.1 in
  { sample; elements; confidence }

let rec classify_all = function
  | [] -> []
  | sample :: rest -> classify_sample sample :: classify_all rest

(* pure recursive grouping *)
let group_by_element classifications =
  let rec add_to_groups element classification = function
    | [] -> [(element, [classification])]
    | (el, cs) :: rest when el = element -> (el, classification :: cs) :: rest
    | group :: rest -> group :: add_to_groups element classification rest in
  
  let process_classification acc classification =
    let rec process_elements acc = function
      | [] -> acc
      | element :: rest_elements ->
        let new_acc = add_to_groups element classification acc in
        process_elements new_acc rest_elements in
    process_elements acc classification.elements in
  
  let rec process_all acc = function
    | [] -> acc
    | c :: rest -> process_all (process_classification acc c) rest in
  
  let grouped = process_all [] classifications in
  List.map (fun (el, cs) -> (el, List.rev cs)) grouped

let element_name = function
  | H2O -> "Water_H2O" | CH4 -> "Methane_CH4" | CO2 -> "Carbon_Dioxide_CO2"
  | NH3 -> "Ammonia_NH3" | H2S -> "Hydrogen_Sulfide_H2S" | SO2 -> "Sulfur_Dioxide_SO2"
  | Unknown s -> s

(* pure csv writing *)
let write_csv_line oc classification =
  let s = classification.sample in
  let first_band = match s.band_bin_center with
    | Some bands when Array.length bands > 0 -> string_of_float bands.(0)
    | _ -> "" in
  let last_band = match s.band_bin_center with  
    | Some bands when Array.length bands > 0 -> 
      string_of_float bands.(Array.length bands - 1)
    | _ -> "" in
  Printf.fprintf oc "%s,%s,%s,%s,%s,%s,%s\n"
    s.filename
    (s.lines |> Option.map string_of_int |> Option.value ~default:"")
    (s.line_samples |> Option.map string_of_int |> Option.value ~default:"")
    (s.bands |> Option.map string_of_int |> Option.value ~default:"")
    first_band last_band
    (classification.confidence |> Option.map string_of_float |> Option.value ~default:"")

let rec write_classifications oc = function
  | [] -> ()
  | c :: rest -> 
    write_csv_line oc c;
    write_classifications oc rest

let write_element_csv (element, classifications) =
  let filename = Printf.sprintf "element_%s.csv" (element_name element) in
  let oc = open_out filename in
  Printf.fprintf oc "Filename,Lines,Samples,Bands,First_Band,Last_Band,Confidence\n";
  write_classifications oc classifications;
  close_out oc;
  Printf.printf "wrote %s with %d samples\n" filename (List.length classifications)

let rec export_all_csvs = function
  | [] -> ()
  | group :: rest ->
    write_element_csv group;
    export_all_csvs rest

(* pure pipeline composition *)
let run_pipeline () =
  Printf.printf "starting SPECML pipeline...\n";
  let files = get_lbl_files () in
  let samples = process_files files in
  let valid = filter_valid samples in
  let classified = classify_all valid in
  let grouped = group_by_element classified in
  
  Printf.printf "found elements in:\n";
  let rec print_summary = function
    | [] -> ()
    | (el, cs) :: rest ->
      Printf.printf "  %s: %d files\n" (element_name el) (List.length cs);
      print_summary rest in
  print_summary grouped;
  
  export_all_csvs grouped;
  Printf.printf "done!\n"

(* pure debug helpers *)
let rec take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | h :: t -> h :: take (n - 1) t

let print_sample s =
  Printf.printf "%s: " s.filename;
  (match s.lines, s.line_samples, s.bands with
   | Some l, Some ls, Some b -> Printf.printf "%dx%dx%d" ls b l
   | _ -> Printf.printf "incomplete");
  (match s.band_bin_center with
   | Some bands when Array.length bands > 0 -> 
     Printf.printf " [%.3f-%.3f µm]" bands.(0) bands.(Array.length bands - 1)
   | _ -> Printf.printf " [no spectral data]");
  Printf.printf "\n"

let rec print_samples = function
  | [] -> ()
  | s :: rest -> 
    print_sample s;
    print_samples rest

(* compatibility *)
let run_export = run_pipeline
