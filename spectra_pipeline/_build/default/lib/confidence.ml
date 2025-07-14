(* confidence.ml - Advanced spectral confidence analysis *)

type element = H2O | CH4 | CO2 | NH3 | H2S | SO2 | Unknown of string

type spectral_feature = {
  element : element;
  center_wavelength : float;
  bandwidth : float;
  expected_depth : float;
  confidence_weight : float;
}

(* enhanced element database with spectral features *)

let spectral_database = [

  (* Water ice - multiple absorption features *)

  { element = H2O; center_wavelength = 1.5; bandwidth = 0.1; expected_depth = 0.8; confidence_weight = 0.9 };
  { element = H2O; center_wavelength = 2.0; bandwidth = 0.15; expected_depth = 0.6; confidence_weight = 0.7 };
  { element = H2O; center_wavelength = 3.0; bandwidth = 0.2; expected_depth = 0.9; confidence_weight = 0.95 };
  
  (* Methane - strong atmospheric lines *)

  { element = CH4; center_wavelength = 1.65; bandwidth = 0.05; expected_depth = 0.7; confidence_weight = 0.85 };
  { element = CH4; center_wavelength = 2.3; bandwidth = 0.1; expected_depth = 0.8; confidence_weight = 0.9 };
  { element = CH4; center_wavelength = 3.3; bandwidth = 0.1; expected_depth = 0.6; confidence_weight = 0.8 };
  
  (* Carbon Dioxide *)

  { element = CO2; center_wavelength = 2.0; bandwidth = 0.1; expected_depth = 0.5; confidence_weight = 0.75 };
  { element = CO2; center_wavelength = 4.3; bandwidth = 0.2; expected_depth = 0.7; confidence_weight = 0.8 };
  
  (* Ammonia *)

  { element = NH3; center_wavelength = 2.0; bandwidth = 0.1; expected_depth = 0.4; confidence_weight = 0.7 };
  { element = NH3; center_wavelength = 2.8; bandwidth = 0.15; expected_depth = 0.6; confidence_weight = 0.75 };
  
  (* Hydrogen sulfide *)

  { element = H2S; center_wavelength = 3.9; bandwidth = 0.1; expected_depth = 0.5; confidence_weight = 0.6 };
  
  (* Sulfur dioxide *)

  { element = SO2; center_wavelength = 4.0; bandwidth = 0.15; expected_depth = 0.6; confidence_weight = 0.7 };
]

let element_name = function
  | H2O -> "Water_H2O" 
  | CH4 -> "Methane_CH4" 
  | CO2 -> "Carbon_Dioxide_CO2"
  | NH3 -> "Ammonia_NH3" 
  | H2S -> "Hydrogen_Sulfide_H2S" 
  | SO2 -> "Sulfur_Dioxide_SO2"
  | Unknown s -> s

(* calculate spectral coverage quality *)

let calculate_coverage_quality bands feature =
  let center = feature.center_wavelength in
  let half_bandwidth = feature.bandwidth /. 2.0 in
  let range_start = center -. half_bandwidth in
  let range_end = center +. half_bandwidth in
  
  let bands_in_range = Array.fold_left (fun acc band ->
    if band >= range_start && band <= range_end then acc + 1 else acc
  ) 0 bands in
  
  let total_bands = Array.length bands in
  let expected_bands_in_range = int_of_float (feature.bandwidth *. float_of_int total_bands /. 5.0) in
  let coverage_ratio = float_of_int bands_in_range /. float_of_int (max 1 expected_bands_in_range) in
  
  min 1.0 coverage_ratio

(* calculate signal-to-noise estimate *)

let estimate_signal_quality bands =
  if Array.length bands < 10 then 0.3
  else
    let mean = Array.fold_left (+.) 0.0 bands /. float_of_int (Array.length bands) in
    let variance = Array.fold_left (fun acc band ->
      let diff = band -. mean in
      acc +. (diff *. diff)
    ) 0.0 bands /. float_of_int (Array.length bands) in
    let noise_level = sqrt variance /. mean in
    
    let quality = 1.0 /. (1.0 +. noise_level *. 10.0) in
    max 0.1 (min 1.0 quality)

(* calculate atmospheric window quality *)

let atmospheric_window_factor wavelength =
  if wavelength >= 0.3 && wavelength <= 0.4 then 0.9
  else if wavelength >= 0.4 && wavelength <= 0.7 then 1.0
  else if wavelength >= 0.7 && wavelength <= 0.9 then 0.95
  else if wavelength >= 1.0 && wavelength <= 1.3 then 0.8
  else if wavelength >= 1.5 && wavelength <= 1.8 then 0.85
  else if wavelength >= 2.0 && wavelength <= 2.4 then 0.9
  else if wavelength >= 3.0 && wavelength <= 4.0 then 0.7
  else if wavelength >= 4.5 && wavelength <= 5.5 then 0.6
  else 0.4

(* calculate instrument calibration confidence *)

let instrument_calibration_factor wavelength =
  if wavelength >= 0.35 && wavelength <= 1.0 then 0.95
  else if wavelength >= 1.0 && wavelength <= 5.1 then 0.9
  else 0.5

(* advanced confidence calculation for a single element *)

let calculate_element_confidence bands element =
  let features = List.filter (fun f -> f.element = element) spectral_database in
  
  if List.length features = 0 then 0.0
  else
    let signal_quality = estimate_signal_quality bands in
    
    let feature_confidences = List.map (fun feature ->
      let coverage = calculate_coverage_quality bands feature in
      let atm_window = atmospheric_window_factor feature.center_wavelength in
      let instrument = instrument_calibration_factor feature.center_wavelength in
      
      let base_confidence = coverage *. atm_window *. instrument *. feature.confidence_weight in
      let adjusted_confidence = base_confidence *. signal_quality in
      
      (adjusted_confidence, feature.expected_depth)
    ) features in
    
    let total_weight = List.fold_left (fun acc (_, depth) -> acc +. depth) 0.0 feature_confidences in
    let weighted_sum = List.fold_left (fun acc (conf, depth) -> acc +. (conf *. depth)) 0.0 feature_confidences in
    
    if total_weight > 0.0 then
      let avg_confidence = weighted_sum /. total_weight in
      let feature_bonus = 1.0 +. (float_of_int (List.length features) -. 1.0) *. 0.1 in
      min 1.0 (avg_confidence *. feature_bonus)
    else 0.0

(* enhanced element detection with confidence *)

let detect_elements_with_confidence bands_opt =
  match bands_opt with
  | None -> []
  | Some bands ->
    let all_elements = [H2O; CH4; CO2; NH3; H2S; SO2] in
    
    List.filter_map (fun element ->
      let confidence = calculate_element_confidence bands element in
      if confidence >= 0.02 then
        Some (element, confidence)
      else None
    ) all_elements

(* simple detection for compatibility *)

let detect_elements bands_opt =
  detect_elements_with_confidence bands_opt |> List.map fst
