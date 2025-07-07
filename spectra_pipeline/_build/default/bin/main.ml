let () =
  Printf.printf "🔍 SPECML debug mode\n";
  let files = Spectra_pipeline.debug_get_lbl_files () in
  Printf.printf "found %d .lbl files\n" (List.length files);
  
  (* Test the first file *)
  let first_file = List.hd files in
  Printf.printf "testing file: %s\n" first_file;
  
  (* Read raw file contents *)
  let ic = open_in first_file in
  Printf.printf "first 10 lines:\n";
  for i = 1 to 10 do
    try
      let line = input_line ic in
      Printf.printf "%d: %s\n" i line
    with End_of_file -> Printf.printf "end of file at line %d\n" i
  done;
  close_in ic
