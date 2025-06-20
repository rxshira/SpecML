(* Step 1: *)
parse_lbl : string -> (string * string) list

v

(* Step 2: *)
extract_metadata : (string * string) list -> label_metadata

v

(* Step 3: *)
read_qub : label_metadata -> float array array array  (* [line][sample][band] *)

v

(* Step 4: *)
analyze_spectrum : float array -> chemical_label option

v

(* Step 5: *)
create_elemental_map : float array array array -> label array array
