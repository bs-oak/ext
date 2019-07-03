module type S = sig
  include Map.S
  
  val safe_find: key -> 'a t -> 'a option

  val from_list: (key * 'a) list -> 'a t

  val union: 'a t -> 'a t -> 'a t

  val merge: 
    (key -> 'a -> 'result -> 'result) ->
    (key -> 'a -> 'b -> 'result -> 'result) ->
    (key -> 'b -> 'result -> 'result) ->
    'a t ->
    'b t ->
    'result ->
    'result

end

module Make (Ord: Map.OrderedType): S with type key = Ord.t