
module N = Bs_internalAVLset
module B =  Bs_Bag
type ('elt, 'id) t0 = ('elt, 'id) N.t0 

type ('elt, 'id)enumeration = 
  ('elt, 'id) N.enumeration0 
=
    End 
  | More of 'elt * ('elt, 'id) t0 * ('elt, 'id) enumeration


let rec add0 ~cmp (t : _ t0) x  : _ t0 =
  match N.toOpt t with 
    None -> N.(return @@ node ~left:empty ~right:empty ~key:x  ~h:1)
  | Some nt ->
    let k = N.key nt in 
    let c = (Bs_Cmp.getCmp cmp) x k [@bs] in
    if c = 0 then t else
    let l,r = N.(left nt, right nt) in 
    if c < 0 then 
      let ll = add0 ~cmp l x in 
      if ll == l then t 
      else N.bal ll k r 
    else 
      let rr = add0 ~cmp r x in 
      if rr == r then t 
      else N.bal l k rr 


(* Splitting.  split x s returns a triple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)
let rec splitAux ~cmp x (n : _ N.node) : _ * bool * _ =   
  let l,v,r = N.(left n , key n, right n) in  
  let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
  if c = 0 then (l, true, r)
  else if c < 0 then
    match N.toOpt l with 
    | None -> 
      N.(empty , false, return n)
    | Some l -> 
      let (ll, pres, rl) = splitAux ~cmp x l in (ll, pres, N.join rl v r)
  else
    match N.toOpt r with 
    | None ->
      N.(return n, false, empty)
    | Some r -> 
      let (lr, pres, rr) = splitAux ~cmp x r in (N.join l v lr, pres, rr)

let rec split0 ~cmp x (t : _ t0) : _ t0 * bool * _ t0 =
  match N.toOpt t with 
    None ->
    N.(empty, false, empty)
  | Some n ->
    splitAux ~cmp x n

let rec mem0 ~cmp  (t: _ t0) x =
  match  N.toOpt t with 
  | None -> false
  | Some n ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp (if c < 0 then N.left n else N.right n) x

let rec remove0 ~cmp (t : _ t0) x : _ t0 = 
  match N.toOpt t with 
    None -> t
  | Some n  ->
    let l,v,r = N.(left n , key n, right n) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then N.merge l r else
    if c < 0 then 
      let ll = remove0 ~cmp  l x in 
      if ll == l then t
      else N.bal ll v r 
    else
      let rr = remove0 ~cmp  r x in 
      if rr == r then t  
      else N.bal l v rr

(** FIXME: provide a [splitAux] which returns a tuple of two instead *)      
let rec union0 ~cmp (s1 : _ t0) (s2 : _ t0) : _ t0=
  match N.(toOpt s1, toOpt s2) with
    (None, _) -> s2
  | (_, None) -> s1
  | Some n1, Some n2 (* (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) *) ->
    let h1, h2 = N.(h n1 , h n2) in                 
    if h1 >= h2 then
      if h2 = 1 then add0 ~cmp s1 (N.key n2)  else begin
        let l1, v1, r1 = N.(left n1, key n1, right n1) in      
        let (l2, _, r2) = split0 ~cmp v1 s2 in
        N.join (union0 ~cmp l1 l2) v1 (union0 ~cmp r1 r2)
      end
    else
    if h1 = 1 then add0 s2 ~cmp (N.key n1)  else begin
      let l2, v2, r2 = N.(left n2 , key n2, right n2) in 
      let (l1, _, r1) = split0 ~cmp v2 s1 in
      N.join (union0 ~cmp l1 l2) v2 (union0 ~cmp r1 r2)
    end

let rec inter0 ~cmp (s1 : _ t0) (s2 : _ t0) =
  match N.(toOpt s1, toOpt s2) with
    (None, _) -> s1
  | (_, None) -> s2
  | Some n1, Some n2 (* (Node(l1, v1, r1, _), t2) *) ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in  
    match splitAux ~cmp v1 n2 with
      (l2, false, r2) ->
      N.concat (inter0 ~cmp l1 l2) (inter0 ~cmp r1 r2)
    | (l2, true, r2) ->
      N.join (inter0 ~cmp l1 l2) v1 (inter0 ~cmp r1 r2)

let rec diff0 ~cmp s1 s2 =
  match N.(toOpt s1, toOpt s2) with
    (None, _) 
  | (_, None) -> s1
  | Some n1, Some n2 (* (Node(l1, v1, r1, _), t2) *) ->
    let l1,v1,r1 = N.(left n1, key n1, right n1) in
    match splitAux ~cmp v1 n2 with
      (l2, false, r2) ->
      N.join (diff0 ~cmp l1 l2) v1 (diff0 ~cmp r1 r2)
    | (l2, true, r2) ->
      N.concat (diff0 ~cmp l1 l2) (diff0 ~cmp r1 r2)



let rec compare_aux ~cmp e1 e2 =
  match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
    let c = (Bs_Cmp.getCmp cmp) v1 v2 [@bs] in
    if c <> 0
    then c
    else compare_aux ~cmp (N.cons_enum r1 e1) (N.cons_enum r2 e2)

let cmp0 ~cmp s1 s2 =
  compare_aux ~cmp (N.cons_enum s1 End) (N.cons_enum s2 End)

let eq0 ~cmp s1 s2 =
  cmp0 ~cmp s1 s2 = 0

let rec subset0 ~cmp (s1 : _ t0) (s2 : _ t0) =
  match N.(toOpt s1, toOpt s2) with
    None, _ ->
    true
  | _, None ->
    false
  | Some t1 , Some t2 (* Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) *) ->
    let l1,v1,r1 = N.(left t1, key t1, right t1) in  
    let l2,v2,r2 = N.(left t2, key t2, right t2) in 
    let c = (Bs_Cmp.getCmp cmp) v1 v2 [@bs] in
    if c = 0 then
      subset0 ~cmp l1 l2 && subset0 ~cmp r1 r2
    else if c < 0 then
      subset0 ~cmp N.(return @@ node ~left:l1 ~key:v1 ~right:empty ~h:0) l2 && subset0 ~cmp r1 s2
    else
      subset0 ~cmp N.(return @@ node ~left:empty ~key:v1 ~right:r1 ~h:0) r2 && subset0 ~cmp l1 s2

let rec findOpt0 ~cmp x (n : _ t0) = 
  match N.toOpt n with 
    None -> None
  | Some t (* Node(l, v, r, _) *) ->
    let v = N.key t in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then Some v
    else findOpt0 ~cmp x (if c < 0 then N.left t else N.right t)

let rec findAssert0 ~cmp x (n : _ t0) =
  match N.toOpt n with 
    None -> [%assert "Not_found"]
  | Some t (* Node(l, v, r, _) *) ->
    let v = N.key t in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then  v
    else findAssert0 ~cmp x (if c < 0 then N.left t else N.right t)

let rec findNull0 ~cmp x (n : _ t0) =
  match N.toOpt n with 
    None -> Js.null
  | Some t (* Node(l, v, r, _) *) ->
    let v = N.key t in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then  N.return v
    else findNull0 ~cmp x (if c < 0 then N.left t else N.right t)    

(* FIXME: use [sorted] attribute *)    
let ofArray0 ~cmp (xs : _ array) : _ t0 =     
  let result = ref N.empty in 
  for i = 0 to Array.length xs - 1 do  
    result := add0 ~cmp !result (Bs_Array.unsafe_get xs i) 
  done ;
  !result 

(* TOOD: optimize heuristics for resizing *)  
let addArray0 ~cmp  h arr =   
  let len = Bs.Array.length arr in 
  let v = ref N.empty0 in  
  for i = 0 to len - 1 do 
    let key = (Bs_Array.unsafe_get arr i) in 
    v := add0 !v  ~cmp key 
  done ;
  !v 


let rec addMutate ~cmp (t : _ t0) (x )=   
  match N.toOpt t with 
  | None -> N.(return @@ node ~left:empty ~right:empty ~key:x ~h:1)
  | Some nt -> 
    let k = N.key nt in 
    let  c = (Bs_Cmp.getCmp cmp) x k [@bs] in  
    if c = 0 then t 
    else
    let l, r = N.(left nt, right nt) in 
    (if c < 0 then                   
       N.leftSet nt (addMutate ~cmp l x)       
     else   
       N.rightSet nt (addMutate ~cmp r x);
     );
    N.return (N.balMutate nt)


