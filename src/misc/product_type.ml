open Base

(* Labels:

   "structs" have string labels: (.x=1, .y="foo", .z=3.1415)
   "tuples" have implicit integer labels: (1, "foo", 3.1415)

   TODO: explicit tuple integer labels? probably not, but maybe (.[0]=1, .[1]="foo", .[2]=3.1415)
   TODO: mixed-label product types? probably! we want these for functions arguments (.x=1, "foo", .z=3.1415)
*)
type field_label = S of string | N of int [@@deriving equal, sexp_of]

(* Case labels:
   
   "tagged unions" have string labels:             Foo(1) :: union { Foo of int | Bar of string }
   "untagged unions" have implicit integer labels: 1      :: union { int | string }
   TODO: mixed-label sumtypes? probably not
   TODO: open vs closed unions
   TODO: a function taking an untagged union is an overload
   TODO: a functcion taking an open tagged union
   TODO: a function taking a closed tagged union   
*)
type case_label = S of string | N of int [@@deriving equal, sexp_of]

type value =
  | Builtin of string
  | Int64 of int64
  | Float64 of float
  | Product of product_value
  | Sum of sum_value

and product_value = {fields: (field_label * value) list}

and sum_value = {label: case_label; value: value}

type typ = Int64 | Float64 | Product of product_type | Sum of sum_type
[@@deriving equal, sexp_of]

and product_type = {fields: (field_label * typ) list}

and sum_type = {cases: (case_label * typ) list}

module Typ = struct
  type t = typ

  let equal = equal_typ
end

module Value = struct
  type t = value

  let rec get_type : value -> typ = function
    | Builtin s -> Int64
    | Int64 _ -> Int64
    | Float64 _ -> Float64
    | Product {fields} ->
        Product {fields= List.map fields ~f:(fun (label, value) -> (label, get_type value))}
    | Sum {label; value} -> Sum {cases= [(label, get_type value)]}
end

let%expect_test "example usage" =
  let point : value = Product {fields= [(S "x", Float64 1.0); (S "y", Float64 2.0)]} in
  let vec2 : typ = Product {fields= [(S "x", Float64); (S "y", Float64)]} in
  Stdio.print_endline @@ Sexp.to_string [%sexp (vec2 : typ)] ;
  [%expect {| (Product((fields(((S x)Float64)((S y)Float64))))) |}] ;
  Stdio.print_endline @@ Sexp.to_string [%sexp (Value.get_type point : typ)] ;
  [%expect {| (Product((fields(((S x)Float64)((S y)Float64))))) |}]

(* let%expect_test "example usage" =
 *   let make_label s = S s in
 *   let make_type n = n in
 *   let point_type =
 *     Product_type.make
 *       [ ( (make_label "x", make_type "Float64") ;
 *           (make_label "y", make_type "Float64") ) ] in
 *   let point : value =
 *     let x = make_scalar_value ("1", "Float64") in
 *     let y = make_scalar_value ("0", "Float64") in
 *     make_product_value [("x", x); ("y", y)] in
 *   Type.equal (get_type point) point_type *)
