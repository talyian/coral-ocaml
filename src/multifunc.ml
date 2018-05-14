open Ast

module StringMap = Map.Make(String)
module NodeSet = Set.Make(
  struct
    let compare = compare
    type t = node
  end)

let rec multifunc deleted scope = function
  | Func func as f -> (
     match StringMap.find_opt func.name !scope with
     | None ->
        let mf = Multifunc (func.name, [f]) in
        scope := StringMap.add func.name mf !scope; [mf]
     | Some(Multifunc (name, funcs)) ->
        let mf = Multifunc (name, (f :: funcs)) in
        scope := StringMap.remove func.name !scope;
        scope := StringMap.add func.name mf !scope;
        [mf]
     | _ -> failwith "unexpected name found")

  | Multifunc (name, funcs) as mf ->
     [mf]
  | Module modinfo ->
     let nodes = modinfo.lines
                 |> List.map (multifunc deleted scope)
                 |> List.concat
                 |> List.filter (fun x -> not (NodeSet.mem x !deleted)) in
     [Module {modinfo with lines=nodes}]
  | n -> [n]

let run = function
  | Module nodes as m ->
     List.hd (multifunc (ref NodeSet.empty) (ref StringMap.empty) m)
  | n -> n
