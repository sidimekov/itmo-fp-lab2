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
end

module Make (Ord : OrderedType) : S with type elt = Ord.t = struct
  type elt = Ord.t

  (* Временная простейшая реализация через список.
     На этапе 3 заменим на настоящее AVL-дерево и перепишем все функции. *)

  type t = elt list

  let empty = []

  let is_empty s = (s = [])

  let mem x s = List.exists (fun y -> Ord.compare x y = 0) s

  let add x s =
    if mem x s then s else x :: s

  let remove x s =
    List.filter (fun y -> Ord.compare x y <> 0) s

  let union a b =
    List.fold_left (fun acc x -> add x acc) a b

  let inter a b =
    List.filter (fun x -> mem x b) a

  let diff a b =
    List.filter (fun x -> not (mem x b)) a

  let cardinal s = List.length s

  let iter f s = List.iter f s

  let fold_left f acc s = List.fold_left f acc s

  let fold_right f s acc = List.fold_right f s acc

  let filter p s = List.filter p s

  let map f s =
    (* NB: на этапе 3 надо будет учесть порядок и дубликаты. *)
    let mapped = List.map f s in
    List.fold_left (fun acc x -> add x acc) empty mapped

  let to_list s =
    (* Для детерминированности отсортируем по Ord.compare *)
    List.sort Ord.compare s

  let of_list l =
    List.fold_left (fun acc x -> add x acc) empty l

  let equal a b =
    let a' = to_list a in
    let b' = to_list b in
    List.length a' = List.length b'
    && List.for_all2 (fun x y -> Ord.compare x y = 0) a' b'

  let compare a b =
    let a' = to_list a in
    let b' = to_list b in
    let rec loop xs ys =
      match xs, ys with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x :: xs', y :: ys' ->
        let c = Ord.compare x y in
        if c <> 0 then c else loop xs' ys'
    in
    loop a' b'
end
