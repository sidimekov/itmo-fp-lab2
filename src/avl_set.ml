module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val remove : elt -> t -> t

  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t

  val cardinal : t -> int
  val iter : (elt -> unit) -> t -> unit

  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val filter : (elt -> bool) -> t -> t
  val map : (elt -> elt) -> t -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val to_list : t -> elt list
  val of_list : elt list -> t

  val min_elt : t -> elt
  val max_elt : t -> elt
end

module Make (Ord : OrderedType) : S with type elt = Ord.t = struct
  type elt = Ord.t

  type t =
    | Empty
    | Node of {
        left : t;
        value : elt;
        right : t;
        height : int;
      }

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let height = function
    | Empty -> 0
    | Node { height; _ } -> height

  let make left value right =
    let hl = height left in
    let hr = height right in
    let h = 1 + max hl hr in
    Node { left; value; right; height = h }

  let balance_factor = function
    | Empty -> 0
    | Node { left; right; _ } -> height left - height right

  let rotate_right = function
    | Node { left = Node { left = a; value = x; right = b; _ }; value = y; right = c; _ } ->
        make a x (make b y c)
    | _ -> failwith "rotate_right: invalid structure"

  let rotate_left = function
    | Node { left = a; value = x; right = Node { left = b; value = y; right = c; _ }; _ } ->
        make (make a x b) y c
    | _ -> failwith "rotate_left: invalid structure"

  let balance t =
    match t with
    | Empty -> Empty
    | Node { left; value; right; _ } as node ->
        let bf = balance_factor node in
        if bf > 1 then
          (* левое поддерево слишком высокое *)
          match left with
          | Empty -> node
          | _ ->
              if balance_factor left < 0 then
                (* большой правый поворот *)
                let left' = rotate_left left in
                rotate_right (make left' value right)
              else
                rotate_right node
        else if bf < -1 then
          (* правое поддерево слишком высокое *)
          match right with
          | Empty -> node
          | _ ->
              if balance_factor right > 0 then
                (* большой левый поворот *)
                let right' = rotate_right right in
                rotate_left (make left value right')
              else
                rotate_left node
        else
          (* уже сбалансировано, просто пересчитываем высоту *)
          make left value right

  let rec mem x = function
    | Empty -> false
    | Node { left; value; right; _ } ->
        let c = Ord.compare x value in
        if c = 0 then true
        else if c < 0 then mem x left
        else mem x right

  let rec add x = function
    | Empty -> make Empty x Empty
    | Node { left; value; right; _ } as node ->
        let c = Ord.compare x value in
        if c = 0 then
          node
        else if c < 0 then
          balance (make (add x left) value right)
        else
          balance (make left value (add x right))

  (* remove_min: вернуть минимальное значение и дерево без него *)
  let rec remove_min = function
    | Empty -> failwith "remove_min on empty"
    | Node { left = Empty; value; right; _ } ->
        (value, right)
    | Node { left; value; right; _ } ->
        let m, left' = remove_min left in
        (m, balance (make left' value right))

  let rec remove x = function
    | Empty -> Empty
    | Node { left; value; right; _ } ->
        let c = Ord.compare x value in
        if c < 0 then
          balance (make (remove x left) value right)
        else if c > 0 then
          balance (make left value (remove x right))
        else
          (* удаляем этот узел *)
          match left, right with
          | Empty, _ -> right
          | _, Empty -> left
          | _ ->
              (* берём минимальный элемент из правого поддерева *)
              let m, right' = remove_min right in
              balance (make left m right')

  let rec iter f = function
    | Empty -> ()
    | Node { left; value; right; _ } ->
        iter f left;
        f value;
        iter f right

  let fold_left f init t =
    let acc = ref init in
    iter (fun x -> acc := f !acc x) t;
    !acc

  let fold_right f t init =
    let rec aux acc = function
      | Empty -> acc
      | Node { left; value; right; _ } ->
          let acc1 = aux acc right in
          aux (f value acc1) left
    in
    aux init t

  let cardinal t =
    fold_left (fun acc _ -> acc + 1) 0 t

  let filter p t =
    fold_left
      (fun acc x -> if p x then add x acc else acc)
      empty t

  let map f t =
    fold_left
      (fun acc x -> add (f x) acc)
      empty t

  let to_list t =
    fold_left (fun acc x -> x :: acc) [] t
    |> List.rev

  let of_list l =
    List.fold_left (fun acc x -> add x acc) empty l

  let union a b =
    if cardinal a <= cardinal b then
      fold_left (fun acc x -> add x acc) b a
    else
      fold_left (fun acc x -> add x acc) a b

  let inter a b =
    fold_left
      (fun acc x -> if mem x b then add x acc else acc)
      empty a

  let diff a b =
    fold_left
      (fun acc x -> if mem x b then acc else add x acc)
      empty a

  let equal a b =
    let la = to_list a in
    let lb = to_list b in
    let rec loop xs ys =
      match xs, ys with
      | [], [] -> true
      | x :: xs', y :: ys' ->
          Ord.compare x y = 0 && loop xs' ys'
      | _ -> false
    in
    loop la lb

  let compare a b =
    let la = to_list a in
    let lb = to_list b in
    let rec loop xs ys =
      match xs, ys with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x :: xs', y :: ys' ->
          let c = Ord.compare x y in
          if c <> 0 then c else loop xs' ys'
    in
    loop la lb

  let rec min_elt = function
  | Empty -> raise Not_found
  | Node { left = Empty; value; _ } -> value
  | Node { left; _ } -> min_elt left

  let rec max_elt = function
  | Empty -> raise Not_found
  | Node { right = Empty; value; _ } -> value
  | Node { right; _ } -> max_elt right
end
