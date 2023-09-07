let woops_sleepy () =
  Miou.call_cc (fun () ->
      (* Woops! Wrong sleep function, we blocked the fiber *)
      Miou.meow "Sleepy task";
      Unix.sleepf 5.;
      Miou_unix.sleep 10.)

let spawn min max =
  (* Some GC action *)
  Miou.meow "Spawning task";
  for _i = 0 to 100 do
    ignore (Sys.opaque_identity @@ Array.init 1000000 float_of_int)
  done;
  let mious = List.init (max - min) (fun i ->
    let i = min + i in
    let m = Miou.call_cc (fun () ->
        Miou.meow ("Spawning task " ^ string_of_int i);
        for _i = 0 to max do
          (* Some more GC action *)
          for _i = 0 to 100 do
            ignore (Sys.opaque_identity @@ Array.init 1000000 float_of_int)
          done;
          Miou_unix.sleep 0.2;
          Miou.yield ()
        done;
        Miou_unix.sleep (float_of_int i))
    in
    Miou_unix.sleep (float_of_int (max - i));
    m)
  in
  mious

(* Based on the Tokio Console example application *)
let main () =
  Miou.meow "Main!";
  let f () = spawn 5 10 in
  let g () = spawn 10 30 in
  let h () = woops_sleepy () in
  Miou.await_all (g () @ f () @ [ h () ] ) |> List.iter (fun v -> assert (v = Ok ()))

let () =
  Miou_unix.run @@ fun () ->
  main ()