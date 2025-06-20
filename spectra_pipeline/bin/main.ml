open Spectra_pipeline

let () =
  let all = parse_all_labels () in
  let typed = all
    |> List.filter_map (fun (file, pairs) -> extract_metadata file pairs)
  in

  List.iter (fun m ->
    Printf.printf "Parsed %s: %dx%dx%d bands, first band @ %.2f µm\n"
      m.filename m.line_samples m.bands m.lines
      (if Array.length m.band_bin_center > 0 then m.band_bin_center.(0) else -1.0)
  ) typed

