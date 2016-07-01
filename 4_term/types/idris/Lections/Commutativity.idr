plc_z : (n : Nat) -> n = n + 0
plc_z Z = Refl
plc_z (S k) =
    let rec = plc_z k
    in rewrite rec in Refl

plc_s : (k : Nat) -> (n : Nat) -> S (k + n) = k + S n
plc_s Z n = Refl
plc_s (S k) n =
    let rec = plc_s k n
    in rewrite rec in Refl

plc : (m : Nat) -> (n : Nat) -> (m + n = n + m)
plc Z n = plc_z n
plc (S k) n =
    let r = plc k n
    in rewrite r in plc_s n k

show_equal : {a : Nat} -> {b : Nat} -> a = b -> Nat
show_equal c = 1

x : (S Z = S Z)
x = Refl
  
main : IO ()
main = do print $ show_equal x
