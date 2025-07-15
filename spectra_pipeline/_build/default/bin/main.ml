(* let () =
  Printf.printf "\nSPECML:\n";
  Spectra_pipeline.run_advanced_pipeline ()


let () =
  Printf.printf "🔍 Checking if spectral data varies between files\n";
  let samples = Spectra_pipeline.get_valid_samples_seq () |> Spectra_pipeline.take_seq 3 in
  List.iteri (fun i sample ->
    Printf.printf "\nFile %d: %s\n" i sample.filename;
    match sample.band_bin_center with
    | Some bands ->
      Printf.printf "  First 3 bands: %.6f, %.6f, %.6f\n" bands.(0) bands.(1) bands.(2);
      Printf.printf "  Last 3 bands: %.6f, %.6f, %.6f\n" 
        bands.(Array.length bands - 3) bands.(Array.length bands - 2) bands.(Array.length bands - 1)
    | None -> Printf.printf "  No bands\n"
  ) samples *)

let () = Spectra_pipeline.debug_spectral_variance ()
