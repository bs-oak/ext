module type S = sig
  include Map.S
  val safe_find: key -> 'a t -> 'a option

  val merge: 
    (key -> 'a -> 'result -> 'result) ->
    (key -> 'a -> 'b -> 'result -> 'result) ->
    (key -> 'b -> 'result -> 'result) ->
    'a t ->
    'b t ->
    'result ->
    'result
end

module Make = functor (Ord: Map.OrderedType) -> struct
  module Map = Map.Make(Ord)
  include Map

  let safe_find k m =
    try Some (Map.find k m)
    with Not_found -> None    
 
  let merge left_step both_step right_step left_map right_map initial_result =
    let rec step_state r_key r_value (list, result) =
      match list with
      | [] -> 
        (list, right_step r_key r_value result)

      | (l_key, l_value) :: rest ->
        if l_key < r_key then
          step_state r_key r_value (rest, left_step l_key l_value result)
        
        else if l_key > r_key then
          (list, right_step r_key r_value result)

        else 
          (rest, both_step l_key l_value r_value result)
    in    
    let leftovers, intermediate_result =
      Map.fold step_state right_map (Map.bindings left_map, initial_result)
    in
    List.fold_left (fun result (k,v) -> left_step k v result ) intermediate_result leftovers
end