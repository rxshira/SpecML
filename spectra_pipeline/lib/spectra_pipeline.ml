(* SPECML with lazy Seq streams - Shira Rubin 2025 *)

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
  elements : Confidence.element list;
  confidence : float option;
  element_confidences : (Confidence.element * float) list;
}

let data_dir = "/Users/mjulia/Documents/specMLData/saturn/finale"

(* safe parsing *)
let safe_int s =
  try Some (int_of_string (String.trim s)) with _ -> None

let safe_float s =
  try Some (float_of_string (String.trim s)) with _ -> None

(* recursive band parsing *)
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

(* lazy stream of .lbl files *)
let lbl_files_seq () =
  let all_files = Sys.readdir data_dir |> Array.to_list in
  Printf.printf "all files count: %d\n" (List.length all_files);
  all_files
  |> List.to_seq
  |> Seq.filter (fun f ->
    let result = Filename.check_suffix f ".lbl" || Filename.check_suffix f ".qub.lbl" in
    if result then Printf.printf "matched: %s\n" f;
    result)
  |> Seq.map (Filename.concat data_dir)

(* fixed multi-line parser *)
let parse_lbl_file filename =
  let rec read_lines ic acc current_key current_value =
    match input_line ic with
    | line ->
      let line = String.trim line in
      if String.contains line '=' then
        let final_acc = match current_key, current_value with
          | Some k, Some v -> (k, v) :: acc
          | _ -> acc in
        (match String.split_on_char '=' line with
        | [key; value] ->
          let key = String.trim key in
          let value = String.trim value in
          if String.ends_with ~suffix:"," value then
            read_lines ic final_acc (Some key) (Some value)
          else
            read_lines ic ((key, value) :: final_acc) None None
        | _ -> read_lines ic final_acc None None)
      else if current_key <> None && current_value <> None then
        let trimmed = String.trim line in
        let new_value = match current_value with
          | Some v -> Some (v ^ trimmed)
          | None -> Some trimmed in
        if String.ends_with ~suffix:")" trimmed || not (String.ends_with ~suffix:"," trimmed) then
          let final_acc = match current_key, new_value with
            | Some k, Some v -> (k, v) :: acc
            | _ -> acc in
          read_lines ic final_acc None None
        else
          read_lines ic acc current_key new_value
      else
        read_lines ic acc current_key current_value
    | exception End_of_file ->
      match current_key, current_value with
      | Some k, Some v -> List.rev ((k, v) :: acc)
      | _ -> List.rev acc in

  try
    let ic = open_in filename in
    let result = read_lines ic [] None None in
    close_in ic;
    Some result
  with _ -> None

(* key lookup *)
let rec find_key key = function
  | [] -> None
  | (k, v) :: t ->
    if String.uppercase_ascii k = String.uppercase_ascii key
    then Some v
    else find_key key t

(* metadata extraction *)
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

(* lazy stream of parsed samples *)
let samples_seq () =
  lbl_files_seq ()
  |> Seq.filter_map (fun filename ->
    match parse_lbl_file filename with
    | None -> None
    | Some pairs -> extract_metadata filename pairs)

(* lazy stream of valid samples *)
let valid_samples_seq () =
  samples_seq ()
  |> Seq.filter (fun sample ->
    match sample.band_bin_center with
    | Some bands -> Array.length bands > 0
    | None -> false)

(* element detection using confidence module *)
let detect_elements = Confidence.detect_elements

(* simple classification function *)
let classify_sample sample =
  let elements = detect_elements sample.band_bin_center in
  let confidence = if List.length elements > 0 then Some 0.8 else Some 0.1 in
  { sample; elements; confidence; element_confidences = [] }

(* lazy stream of classifications *)
let classifications_seq () =
  valid_samples_seq ()
  |> Seq.map classify_sample

(* advanced classification using confidence module *)
let classify_sample_advanced sample =
  let element_confidences = Confidence.detect_elements_with_confidence sample.band_bin_center in
  
  if List.length element_confidences = 0 then
    { sample; elements = []; confidence = Some 0.05; element_confidences = [] }
  else
    let elements = List.map fst element_confidences in
    let confidences = List.map snd element_confidences in
    
    let avg_confidence = List.fold_left (+.) 0.0 confidences /. float_of_int (List.length confidences) in
    { sample; elements; confidence = Some avg_confidence; element_confidences }

(* convert lazy stream to grouped results *)
let group_by_element_seq classifications_seq =
  let element_map = Hashtbl.create 10 in

  classifications_seq
  |> Seq.iter (fun classification ->
    List.iter (fun element ->
      let existing = Hashtbl.find_opt element_map element |> Option.value ~default:[] in
      Hashtbl.replace element_map element (classification :: existing)
    ) classification.elements);

  Hashtbl.fold (fun element classifications acc ->
    (element, List.rev classifications) :: acc
  ) element_map []

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
  let filename = Printf.sprintf "element_%s.csv" (Confidence.element_name element) in
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

(* Lazy pipeline composition *)
let run_seq_pipeline () =
  Printf.printf "starting SPECML lazy pipeline...\n";
  let total_files = lbl_files_seq () |> Seq.length in
  Printf.printf "filtered lbl files: %d\n" total_files;

  let classifications = classifications_seq () in
  let grouped = group_by_element_seq classifications in

  Printf.printf "found elements in:\n";
  let rec print_summary = function
    | [] -> ()
    | (el, cs) :: rest ->
      Printf.printf "  %s: %d files\n" (Confidence.element_name el) (List.length cs);
      print_summary rest in
  print_summary grouped;

  export_all_csvs grouped;
  Printf.printf "done!!\n"

(* advanced pipeline with confidence scores *)
let run_advanced_pipeline () =
  Printf.printf "starting SPECML advanced pipeline...\n";
  let total_files = lbl_files_seq () |> Seq.length in
  Printf.printf "filtered lbl files: %d\n" total_files;

  let advanced_classifications = valid_samples_seq () |> Seq.map classify_sample_advanced in
  let grouped = group_by_element_seq advanced_classifications in

  Printf.printf "found elements in:\n";
  let rec print_summary = function
    | [] -> ()
    | (el, cs) :: rest ->
      Printf.printf "  %s: %d files (avg confidence: %.2f)\n" 
        (Confidence.element_name el) 
        (List.length cs)
        (List.fold_left (fun acc c -> acc +. (Option.value c.confidence ~default:0.0)) 0.0 cs 
         /. float_of_int (List.length cs));
      print_summary rest in
  print_summary grouped;

  export_all_csvs grouped;
  Printf.printf "done!!\n"

(* debug helpers that work with sequences *)
let take_seq n seq = seq |> Seq.take n |> List.of_seq

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

let print_samples samples = List.iter print_sample samples

(* debug first few samples *)
let debug_seq_samples n =
  Printf.printf "debug: first %d samples from lazy stream\n" n;
  let samples = samples_seq () |> take_seq n in
  print_samples samples

(* compatibility *)
let run_pipeline = run_advanced_pipeline  (* use advanced by default *)
let run_export = run_advanced_pipeline

(* expose sequences for debugging *)
let get_samples_seq = samples_seq
let get_valid_samples_seq = valid_samples_seq
let get_classifications_seq = classifications_seq

(* expose for debugging *)
let classify_sample = classify_sample
let classify_sample_advanced = classify_sample_advanced

(* debug confidence scores *)
let debug_confidence_scores sample =
  match sample.band_bin_center with
  | Some bands ->
    Printf.printf "Sample has %d bands\n" (Array.length bands);
    
    (* Test each element's confidence manually *)
    let elements = [("H2O", Confidence.H2O); ("CH4", Confidence.CH4); ("CO2", Confidence.CO2)] in
    List.iter (fun (name, el) ->
      let conf = Confidence.calculate_element_confidence bands el in
      Printf.printf "%s confidence: %.3f\n" name conf
    ) elements
  | None -> Printf.printf "No bands\n"
