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
        let mf = Multifunc {name=func.name; func=ref f; next=None} in
        scope := StringMap.add func.name mf !scope; [mf]
     | Some(Multifunc data as mf) ->
        let mf = Multifunc {data with next=Some(ref mf);func=ref f} in
        scope := StringMap.remove func.name !scope;
        scope := StringMap.add func.name mf !scope;
        [mf]
     | _ -> failwith "unexpected name found")

  | Multifunc _ as mf -> [mf]
  | Module modinfo ->
     let nodes = modinfo.lines
                 |> List.map (multifunc deleted scope)
                 |> List.concat
                 |> List.filter (fun x -> not (NodeSet.mem x !deleted)) in
     [Module {modinfo with lines=nodes}]
  | n -> [n]

let run = function
  | Module _ as m ->
     List.hd (multifunc (ref NodeSet.empty) (ref StringMap.empty) m)
  | n -> n
