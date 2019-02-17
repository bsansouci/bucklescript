type ('k, 'v) node  = {
  mutable key : 'k [@bs.optional];
  mutable value : 'v;
  mutable height : int;
  mutable left : ('k,'v) t;
  mutable right : ('k,'v) t
}
and ('key, 'a) t = ('key, 'a) node option
[@@bs.deriving abstract]


let create x y =
  Some (node ~left:None ~key:x ~value:y ~right:None ~height:1 ())
