module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t
end

module Make (Ord : OrderedType) : S with type elt = Ord.t
