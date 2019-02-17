type ('key, 'a) t = ('key, 'a) node option

and ('k,  'v) node  = private {
  mutable key : 'k [@bs.optional];
  mutable value : 'v;
  height : int;
  mutable left : ('k,'v) t;
  mutable right : ('k,'v) t
} [@@bs.deriving abstract]

val create : 'a -> 'b -> ('a,'b) t
