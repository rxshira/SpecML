let () =
 Printf.printf "🔍 Checking wavelength ranges\n";
 let files = Spectra_pipeline.get_lbl_files () in
 let samples = Spectra_pipeline.process_files (Spectra_pipeline.take 1 files) in
 Spectra_pipeline.print_samples samples
