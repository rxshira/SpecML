(* open Spectra_pipeline *)
(*
let () =
  let parsed = Spectra_pipeline.parse_all_labels () in
  let count = ref 0 in
  let skipped = ref 0 in
  List.iter (fun (file, pairs) ->
    match Spectra_pipeline.extract_metadata file pairs with
    | Some m ->
        incr count;
        let first_band =
          if Array.length m.band_bin_center > 0 then m.band_bin_center.(0)
          else nan
        in
        Printf.printf "Parsed %s: %dx%dx%d bands, first band @ %.2f µm\n"
          m.filename m.line_samples m.bands m.lines first_band
    | None -> incr skipped
  ) parsed;
  Printf.printf "\n✔ Parsed %d files, ❌ Skipped %d files\n" !count !skipped
*)

let () =
  Spectra_pipeline.run_export ()

