let pow (a: float) (n: int) : float = 
  let rec go acc a n =
    if n < 0 then go acc (1. /. a) (-n)
    else if n = 1 then acc *. a
    else if n mod 2 = 0 then go acc (a *. a) (n/2)
    else go (acc *. a) (a *. a) ((n-1)/2)
  in
  if n = 0 then 1.
  else go 1. a n

(* multiplication by doubling *)
let multiply x y = 
  let rec go acc x y =
    if y = 1 then acc + x
    else if y mod 2 = 0 then go acc (x+x) (y/2)
    else go (acc + x) (x+x) ((y-1)/2)
  in
  if (x = 0 || y = 0) then 0
  else if x = 1 then y
  else go 0 x y

(* multiplication using for loop *)
let multiply' a b = 
  if (a = 0 || b = 0) then
    0
  else (
    let product = ref 0 in
    for i = 1 to b do
      product := !product + a
    done;
    !product
  )

(* multiplication using tail recursion *)
let multiply'' a b =
  let rec go acc b =
    if b = 1 then acc + a
    else go (acc + a) (b-1)
  in
  if (a = 0 || b = 0) then 0
  else if b < 0 then -(go 0 (-b))
  else go 0 b

(* multiplication not using *, /, nor inner function *)
let rec multiply''' a b =
  if (a = 0 || b = 0) then 0
  else if b = 1 then a
  else if b < 0 then -(multiply''' a (-b))
  else a + multiply''' a (b-1)
