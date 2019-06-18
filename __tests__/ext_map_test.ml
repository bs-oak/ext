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


)