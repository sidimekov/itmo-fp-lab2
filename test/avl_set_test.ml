module IntOrd = struct
  type t = int
  let compare = Int.compare
end

module IntSet = Avl_set.Make (IntOrd)

open Alcotest

(* Unit-тесты  *)

let test_empty () =
  check bool "empty is empty" true (IntSet.is_empty IntSet.empty)

let test_add_mem () =
  let s = IntSet.empty |> IntSet.add 1 |> IntSet.add 2 in
  check bool "mem 1" true (IntSet.mem 1 s);
  check bool "mem 2" true (IntSet.mem 2 s);
  check bool "mem 3" false (IntSet.mem 3 s)

let test_add_no_duplicates () =
  let s = IntSet.empty |> IntSet.add 1 |> IntSet.add 1 in
  check int "cardinal after dup adds" 1 (IntSet.cardinal s)

let test_remove () =
  let s = IntSet.(empty |> add 1 |> add 2 |> add 3) in
  let s' = IntSet.remove 2 s in
  check bool "2 removed" false (IntSet.mem 2 s');
  check bool "1 still there" true (IntSet.mem 1 s');
  check bool "3 still there" true (IntSet.mem 3 s')

let test_of_list_to_list_sorted () =
  let s = IntSet.of_list [ 3; 1; 2; 2 ] in
  let lst = IntSet.to_list s in
  check (list int) "sorted unique" [ 1; 2; 3 ] lst

let unit_tests =
  [
    test_case "empty" `Quick test_empty;
    test_case "add/mem" `Quick test_add_mem;
    test_case "add no duplicates" `Quick test_add_no_duplicates;
    test_case "remove" `Quick test_remove;
    test_case "of_list/to_list" `Quick test_of_list_to_list_sorted;
  ]

(* Property-based тесты *)

open QCheck

let gen_int_set =
  map IntSet.of_list (list small_int)

(* Свойства моноида: empty - нейтральный элемент для union *)

let prop_monoid_left_identity =
  Test.make
    ~name:"union empty s = s"
    gen_int_set
    (fun s -> IntSet.equal (IntSet.union IntSet.empty s) s)

let prop_monoid_right_identity =
  Test.make
    ~name:"union s empty = s"
    gen_int_set
    (fun s -> IntSet.equal (IntSet.union s IntSet.empty) s)

let gen3_int_set =
  triple gen_int_set gen_int_set gen_int_set

let prop_monoid_associativity =
  Test.make
    ~name:"union associative"
    gen3_int_set
    (fun (a, b, c) ->
       let left = IntSet.union (IntSet.union a b) c in
       let right = IntSet.union a (IntSet.union b c) in
       IntSet.equal left right)

(* Дополнительные свойства множества *)

let prop_cardinal_le_list_length =
  Test.make
    ~name:"cardinal (of_list l) <= length l"
    (list small_int)
    (fun l ->
       let s = IntSet.of_list l in
       IntSet.cardinal s <= List.length l)

let prop_union_idempotent =
  Test.make
    ~name:"union s s = s"
    gen_int_set
    (fun s -> IntSet.equal (IntSet.union s s) s)

let prop_mem_of_list =
  Test.make
    ~name:"mem x (of_list l) <-> List.mem x l"
    (pair small_int (list small_int))
    (fun (x, l) ->
       let s = IntSet.of_list l in
       IntSet.mem x s = List.mem x l)

let qcheck_tests =
  [
    QCheck_alcotest.to_alcotest prop_monoid_left_identity;
    QCheck_alcotest.to_alcotest prop_monoid_right_identity;
    QCheck_alcotest.to_alcotest prop_monoid_associativity;
    QCheck_alcotest.to_alcotest prop_cardinal_le_list_length;
    QCheck_alcotest.to_alcotest prop_union_idempotent;
    QCheck_alcotest.to_alcotest prop_mem_of_list;
  ]

(*  Запуск тестов  *)

let () =
  Alcotest.run "avl_set"
    [
      ("unit", unit_tests);
      ("properties", qcheck_tests);
    ]
