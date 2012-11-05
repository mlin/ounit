(*
 * Logger for information and various OUnit events.
 *)

open OUnitTypes
open OUnitUtils

type event_type = GlobalEvent of global_event | TestEvent of test_event
type verbosity = Off | On | PreTTY

let cur_path = ref []
let t0 = ref (Unix.gettimeofday ())

let format_event verbose event_type =
  match event_type with
    | GlobalEvent e ->
        begin
          match e with 
            | GStart ->
                ""
            | GEnd ->
                ""
            | GResults (running_time, results, test_case_count) -> 
                let separator1 = String.make (Format.get_margin ()) '=' in
                let separator2 = String.make (Format.get_margin ()) '-' in
                let buf = Buffer.create 1024 in
                let bprintf fmt = Printf.bprintf buf fmt in
                let print_results = 
                  List.iter 
                    (fun result -> 
                       bprintf "%s\n%s: %s\n\n%s\n%s\n" 
                         separator1 
                         (result_flavour result) 
                         (string_of_path (result_path result)) 
                         (result_msg result) 
                         separator2)
                in
                let errors   = List.filter is_error results in
                let failures = List.filter is_failure results in
                let skips    = List.filter is_skip results in
                let todos    = List.filter is_todo results in

                  bprintf "\n";

                  print_results errors;
                  print_results failures;
                  bprintf "Ran: %d tests in: %.2f seconds.\n" 
                    (List.length results) running_time;

                  (* Print final verdict *)
                  if was_successful results then 
                    begin
                      if skips = [] then
                        bprintf "OK"
                      else 
                        bprintf "OK: Cases: %d Skip: %d"
                          test_case_count (List.length skips)
                    end
                  else
                    begin
                      bprintf
                        "FAILED: Cases: %d Tried: %d Errors: %d \
                              Failures: %d Skip:%d Todo:%d" 
                        test_case_count (List.length results) 
                        (List.length errors) (List.length failures)
                        (List.length skips) (List.length todos);
                    end;
                  bprintf "\n";
                  Buffer.contents buf
        end

    | TestEvent e ->
        begin
          let string_of_result = 
            match verbose with
            | PreTTY -> begin
               function
                | RSuccess _      -> "\x1B[92m\xE2\x9C\x93\x1B[0m "
                | RFailure (_, _) -> "\x1B[91m\xD7[0m "
                | RError (_, _)   -> "ERROR"
                | RSkip (_, _)    -> "SKIP"
                | RTodo (_, _)    -> "TODO"
              end
            | On -> begin
              function
                | RSuccess _      -> "ok"
                | RFailure (_, _) -> "FAIL"
                | RError (_, _)   -> "ERROR"
                | RSkip (_, _)    -> "SKIP"
                | RTodo (_, _)    -> "TODO"
              end
            | Off -> begin
              function
                | RSuccess _      -> "."
                | RFailure (_, _) -> "F"
                | RError (_, _)   -> "E"
                | RSkip (_, _)    -> "S"
                | RTodo (_, _)    -> "T"
              end
          in
            if verbose <> Off then
              match e with 
                | EStart p -> 
                    cur_path := p; t0 := Unix.gettimeofday ();
                    Printf.sprintf "    %s%s" (string_of_path p) (if verbose <> PreTTY then "\n" else "")
                | EEnd p -> ""
                | EResult result ->
                    let elapsed = (int_of_float (1000.0 *. (Unix.gettimeofday() -. !t0))) in
                    let elapsed_str =
                      if verbose = PreTTY then
                        Printf.sprintf "\x1B[90m(%dms)\x1B[0m" elapsed
                      else
                        Printf.sprintf "(%dms)" elapsed
                    in
                    Printf.sprintf "%s %s %s %s\n"
                      (if verbose = PreTTY then "\r" else "") (* on terminal, overwrite EStart line *)
                      (string_of_result result)
                      ((if verbose = PreTTY then string_of_path_PreTTY else string_of_path) !cur_path)
                      elapsed_str
                    (* TODO: would be nicer to refactor cur_path and t0 into EResult instead of using global vars*)
                | ELog (lvl, str) ->
                    let prefix = 
                      match lvl with 
                        | LError -> "E"
                        | LWarning -> "W"
                        | LInfo -> "I"
                    in
                      prefix^": "^str
                | ELogRaw str ->
                    str
            else 
              match e with 
                | EStart _ | EEnd _ | ELog _ | ELogRaw _ -> ""
                | EResult result -> string_of_result result
        end

let file_logger fn =
  let chn = open_out fn in
    (fun ev ->
       output_string chn (format_event On ev);
       flush chn),
    (fun () -> close_out chn)

let std_logger verbose =
  (fun ev -> 
     print_string (format_event (if verbose then (if Unix.isatty Unix.stdout then PreTTY else On) else Off) ev);
     flush stdout),
  (fun () -> ())

let null_logger =
  ignore, ignore

let create output_file_opt verbose (log,close) =
  let std_log, std_close = std_logger verbose in
  let file_log, file_close = 
    match output_file_opt with 
      | Some fn ->
          file_logger fn
      | None ->
          null_logger
  in
    (fun ev ->
       std_log ev; file_log ev; log ev),
    (fun () ->
       std_close (); file_close (); close ())

let printf log fmt =
  Printf.ksprintf
    (fun s ->
       log (TestEvent (ELogRaw s)))
    fmt
