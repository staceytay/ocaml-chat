open Core.Std
open Async.Std

(* TODO: Refactor this to be UI-independent. *)
let receive_message r =
  Reader.read_line r >>= function
  | `Eof -> failwith "Unexpected input from client.\n"
  | `Ok message ->
    (* TODO: Return ACK. *)
    print_endline message; return ()

(* TODO: Refactor this to be UI-independent. *)
let send_message w =
  let stdin = Lazy.force Reader.stdin in
  Reader.read_line stdin >>= function
  | `Eof -> failwith "Unexpected input from stdin.\n"
  | `Ok line -> begin
      (* TODO: Measure actual roundtrip time. *)
      let start = Unix.gettimeofday () in
      Writer.write_line w line;
      let time_taken = (Unix.gettimeofday ()) -. start in
      printf "Roundtrip time: %f seconds\n" time_taken;
      return ()
    end

let read_and_send_message _ r w =
  let rec loop r w =
    Deferred.all [
      (receive_message r);
      (send_message w);
    ] >>= (fun _ -> loop r w)
  in loop r w

let run ~port =
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.on_port port)
    read_and_send_message
  >>= fun _ -> Deferred.never ()

let () =
  Command.async
    ~summary:"Start a 1:1 chat server"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 8080 int)
        ~doc: " Port to listen on (default 8080)"
    )
    (fun port () -> run ~port)
  |> Command.run
