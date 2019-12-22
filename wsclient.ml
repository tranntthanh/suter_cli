open Lwt.Infix
open Websocket
open Connection
open Config
open Dsl
open Console

let init_command_db send recv : 'a option Lwt.t=
  let open Command in
  let m = "state_getMetadata" in
  let cmd = compose_unsafe m None 1 in

  let db = CommandDB.TypeDb.empty in
  let%lwt result = execute_command send recv cmd in
  let%lwt _ = io_printf "> entering script mode:\n> " in
  (* TODO construct the formal db here *)
  Lwt.return @@ Some (dft_cdb db)

let zkp_client uri =

  let open Command in

  let%lwt recv, send = setup_connection uri in
  let%lwt db = init_command_db send recv in

  let rec react_forever env = match db with
    | None -> begin
        let%lwt _ = Lwt_log.debug ~section
        "Exit on error." in
        send @@ Frame.close 1000
      end
    | Some db -> begin
        Lwt_io.(read_line_opt stdin) >>= function
        | None -> begin
            let%lwt _ = Lwt_log.debug ~section
	        "Got EOF. Sending a close frame." in
            send @@ Frame.close 1000
          end
        | Some cmd -> begin
            let%lwt _ = try
                let lvl, cmd = parse_arg @@ Stream.of_string cmd in
                let remote_call = execute_command send recv in
                Rt.ExecuteClosure.run db lvl cmd remote_call
            with 
            | Ploc.Exc (loc, e) ->
                io_printf "> Syntax Error: start %d, end %d\n> "
                    (Ploc.first_pos loc) (Ploc.last_pos loc)
            | TypingError (t,a) ->
                io_printf "> Command Typing Error: %s is expected to have type %s"
                (Arg.to_string a) (ArgType.to_string t)
            in
            react_forever env
          end
      end

  in react_forever JSONClosure.empty

let apply_loglevel = function
| 2 -> Lwt_log.(add_rule "*" Info)
| 3 -> Lwt_log.(add_rule "*" Debug)
| _ -> ()

let () =
  let reset = ref false in
  let uri = ref "" in
  let cmd = ref "" in
  let set_uri s = uri := s in
  let fetch_cmd s = cmd := s in
  let spec_args = Arg.align [
      "-loglevel", Arg.Int apply_loglevel, "1-3 Set loglevel";
      "-r", Arg.Set reset, " ZKP reset sign process";
      "-url", Arg.String set_uri, " ZKP reset sign process";
    ] in
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> uri\nOptions are:" in
  Arg.parse spec_args fetch_cmd usage_msg;
  Lwt_main.run (zkp_client (Uri.of_string !uri))
