let () =
  Printf.printf "\nSPECML:\n";
  Spectra_pipeline.run_advanced_pipeline ()

(*
let () =
  let samples = Spectra_pipeline.get_valid_samples_seq () |> Spectra_pipeline.take_seq 1 in
  if List.length samples > 0 then (
    let sample = List.hd samples in
    Spectra_pipeline.debug_element_confidence_detailed sample "H2O";
    Spectra_pipeline.debug_element_confidence_detailed sample "CH4"
  )

*)
