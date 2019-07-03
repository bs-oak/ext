open Jest
open Expect

module StringMap = Ext_map.Make(String)

let () = describe "Ext_map" (fun () -> 
    describe "#safe_find" (fun () ->  
        test "some value when key exists" (fun () -> 
            let map = 
                StringMap.empty
                |> StringMap.add "mykey" "myval"
            in
            expect (StringMap.safe_find "mykey" map) |> toEqual( Some "myval" )
        );
        test "none when key doesn't exist" (fun () -> 
            let map = 
                StringMap.empty
                |> StringMap.add "mykey" "myval"
            in
            expect (StringMap.safe_find "xxxx" map) |> toEqual( None )
        );        
    );

    describe "#merge" (fun () ->  
        test "correctly merges" (fun () -> 
            let map1 = 
                StringMap.empty
                |> StringMap.add "day" "day"
                |> StringMap.add "color" "color"                
            in

            let map2 = 
                StringMap.empty
                |> StringMap.add "fruit" "fruit"
                |> StringMap.add "day" "day"
            in
            
            let left_step k _v acc = (k ^ "-left") :: acc in
            let both_step k _v1 _v2 acc = (k ^ "-both") :: acc in
            let right_step k _v acc = (k ^ "-right") :: acc in

            expect (StringMap.merge left_step both_step right_step map1 map2 []) |> toEqual( ["fruit-right";"day-both";"color-left"] )
        );     
    );

    describe "#from_list" (fun () ->  
        test "returns map from list" (fun () -> 
            expect (
                ["a", 1; "b", 2; "c", 3; "b", 4]
                |> StringMap.from_list
            ) 
            |> toEqual( 
                StringMap.empty
                |> StringMap.add "a" 1
                |> StringMap.add "b" 4
                |> StringMap.add "c" 3
            )
        )
    );

     describe "#union" (fun () ->  
        test "joins to maps" (fun () -> 
            let t1 =
                StringMap.empty
                |> StringMap.add "a" 1
                |> StringMap.add "b" 2
            in

            let t2 =
                StringMap.empty
                |> StringMap.add "c" 3
                |> StringMap.add "d" 4
            in       

            let t3 =
                StringMap.empty
                |> StringMap.add "a" 1
                |> StringMap.add "b" 2
                |> StringMap.add "c" 3
                |> StringMap.add "d" 4
            in     

            expect (StringMap.union t1 t2 |> StringMap.bindings) |> toEqual (StringMap.bindings t3)
        );
        test "key collisions give preference to t1" (fun () -> 
            let t1 =
                StringMap.empty
                |> StringMap.add "a" 1
                |> StringMap.add "b" 2
            in

            let t2 =
                StringMap.empty
                |> StringMap.add "b" 3
                |> StringMap.add "c" 4
            in       

            let t3 =
                StringMap.empty
                |> StringMap.add "a" 1
                |> StringMap.add "b" 2
                |> StringMap.add "c" 4
            in     

            expect (StringMap.union t1 t2 |> StringMap.bindings) |> toEqual (StringMap.bindings t3)
        )
    )
)