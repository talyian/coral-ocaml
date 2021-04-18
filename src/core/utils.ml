open Base

let show_table list =
  let lengths = List.init ~f:(Fn.const 0) (List.length @@ List.hd_exn list) in
  let lengths =
    List.fold ~init:lengths
      ~f:(fun lengths row ->
        List.zip_exn lengths row |> List.map ~f:(fun (a, b) -> max a (String.length b)))
      list in
  List.iter
    ~f:(fun row ->
      List.iteri row ~f:(fun i cell ->
          Stdio.print_string
          @@ String.prefix (cell ^ String.make 100 ' ') (1 + List.nth_exn lengths i)) ;
      Stdio.print_endline "")
    list
