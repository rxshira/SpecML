(* confidence.mli - interface for spectral confidence analysis *)

type element = H2O | CH4 | CO2 | NH3 | H2S | SO2 | Unknown of string

(* convert element to string name *)

val element_name : element -> string

(* simple element detection (for compatibility) *)

val detect_elements : float array option -> element list

(* advanced element detection with confidence scores *)

val detect_elements_with_confidence : float array option -> (element * float) list

(* calculate confidence for a specific element *)

val calculate_element_confidence : float array -> element -> float
