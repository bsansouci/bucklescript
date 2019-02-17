include
  (struct
     type ('k,'v) node =
       {
       mutable key: 'k option[@bs.optional ];
       mutable value: 'v;
       mutable height: int;
       mutable left: ('k,'v) t;
       mutable right: ('k,'v) t;}
     and ('key,'a) t = ('key,'a) node option
     let node:
       ?key:'k ->
         value:'v ->
           height:int ->
             left:('k,'v) t -> right:('k,'v) t -> unit -> ('k,'v) node
       =
       fun ?key  ->
         fun ~value  ->
           fun ~height  ->
             fun ~left  ->
               fun ~right  -> fun ()  -> { key; value; height; left; right }
     let keySet: ('k,'v) node -> 'k -> unit =
       fun o  -> fun v  -> o.key <- Some v
     let key: ('k,'v) node -> 'k option = fun o  -> o.key[@@ocaml.deprecated
                                                           "use keyGet instead or use {abstract = light} explicitly"]
     let keyGet: ('k,'v) node -> 'k option = fun o  -> o.key
     let valueSet: ('k,'v) node -> 'v -> unit =
       fun o  -> fun v  -> o.value <- v
     let value: ('k,'v) node -> 'v = fun o  -> o.value[@@ocaml.deprecated
                                                        "use valueGet instead or use {abstract = light} explicitly"]
     let valueGet: ('k,'v) node -> 'v = fun o  -> o.value
     let heightSet: ('k,'v) node -> int -> unit =
       fun o  -> fun v  -> o.height <- v
     let height: ('k,'v) node -> int = fun o  -> o.height[@@ocaml.deprecated
                                                           "use heightGet instead or use {abstract = light} explicitly"]
     let heightGet: ('k,'v) node -> int = fun o  -> o.height
     let leftSet: ('k,'v) node -> ('k,'v) t -> unit =
       fun o  -> fun v  -> o.left <- v
     let left: ('k,'v) node -> ('k,'v) t = fun o  -> o.left[@@ocaml.deprecated
                                                             "use leftGet instead or use {abstract = light} explicitly"]
     let leftGet: ('k,'v) node -> ('k,'v) t = fun o  -> o.left
     let rightSet: ('k,'v) node -> ('k,'v) t -> unit =
       fun o  -> fun v  -> o.right <- v
     let right: ('k,'v) node -> ('k,'v) t = fun o  -> o.right[@@ocaml.deprecated
                                                               "use rightGet instead or use {abstract = light} explicitly"]
     let rightGet: ('k,'v) node -> ('k,'v) t = fun o  -> o.right
   end :
    sig
      type ('k,'v) node
      and ('key,'a) t = ('key,'a) node option
      val node :
        ?key:'k ->
          value:'v ->
            height:int ->
              left:('k,'v) t -> right:('k,'v) t -> unit -> ('k,'v) node
      val keySet : ('k,'v) node -> 'k -> unit
      val key : ('k,'v) node -> 'k option
      val keyGet : ('k,'v) node -> 'k option
      val valueSet : ('k,'v) node -> 'v -> unit
      val value : ('k,'v) node -> 'v
      val valueGet : ('k,'v) node -> 'v
      val heightSet : ('k,'v) node -> int -> unit
      val height : ('k,'v) node -> int
      val heightGet : ('k,'v) node -> int
      val leftSet : ('k,'v) node -> ('k,'v) t -> unit
      val left : ('k,'v) node -> ('k,'v) t
      val leftGet : ('k,'v) node -> ('k,'v) t
      val rightSet : ('k,'v) node -> ('k,'v) t -> unit
      val right : ('k,'v) node -> ('k,'v) t
      val rightGet : ('k,'v) node -> ('k,'v) t
    end)
let create x y =
  Some (node ~left:None ~key:x ~value:y ~right:None ~height:1 ())
