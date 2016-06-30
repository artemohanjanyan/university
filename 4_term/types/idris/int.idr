-- R рефлексивно, если для любого (x : A) мы можем создать (R x x)
Reflx : {A : Type} -> (R : A -> A -> Type) -> Type
Reflx {A} R = (x : A) -> R x x

-- R симметрично, если из (R x y) мы можем создать (R y x)
Sym : {A : Type} -> (R : A -> A -> Type) -> Type
Sym {A} R = (x : A) -> (y : A) -> R x y -> R y x

-- R транзитивно, если из (R x y) и (R y z) мы можем создать (R x z)
Trans : {A : Type} -> (R : A -> A -> Type) -> Type
Trans {A} R = (x : A) -> (y : A) -> (z : A) -> R x y -> R y z -> R x z

-- Отношение эквивалентности -- это рефлексивное, симметричное и транзитивное отношение
data IsEquivalence : {A : Type} -> (R : A -> A -> Type) -> Type where
    EqProof : {A : Type} -> (R : A -> A -> Type) -> Reflx {A} R -> Sym {A} R -> Trans {A} R -> IsEquivalence {A} R

-- Сетоид -- это множество с заданым на ним отношением эквивалентности
record Setoid where
    constructor MkSetoid
    Carrier : Type
    Equiv : Carrier -> Carrier -> Type
    EquivProof : IsEquivalence Equiv

-- Определяем тип Int через Nat
-- Sub a b задаёт число a - b
data MyInt = Sub Nat Nat

-- Определяем равенство
-- a - b == c - d  <===>  a + d == c + b
data IntEq : MyInt -> MyInt -> Type where
    IntRefl : (eq : a + d = c + b) -> IntEq (Sub a b) (Sub c d)

intRefl : Reflx IntEq
intRefl (Sub a b) = IntRefl $ Refl {x = a + b}

intSym : Sym IntEq
intSym (Sub a b) (Sub c d) (IntRefl eq) = IntRefl $ sym eq

intTrans : Trans IntEq
intTrans (Sub a b) (Sub c d) (Sub e f) (IntRefl eq1) (IntRefl eq2) =
    -- Путём несложных преобразований...
    let eq3 = reflPlusRefl eq1 eq2
        elD1 = plusCommutative (a + d) (c + f) -- el for eliminate
        elD2 = plusAssociative (c + f) a d
        elD3 = plusAssociative (c + b) e d
        elD4 = trans (sym elD2) $ trans (sym elD1) $ trans eq3 elD3
        elD = plusRightCancel ((c + f) + a) ((c + b) + e) d elD4
        elC1 = trans (plusAssociative c f a) elD
        elC2 = trans (plusCommutative (f + a) c) elC1
        elC3 = trans elC2 $ sym $ plusAssociative c b e
        elC4 = trans elC3 $ plusCommutative c (b + e)
        elC = plusRightCancel (f + a) (b + e) c elC4
        rev1 = trans (plusCommutative a f) elC
        rev2 = trans rev1 $ plusCommutative b e
    in IntRefl rev2 -- тут можно поставить вопрос и спросить его тип, чтобы понятнее стало
  where
    reflPlusRefl : {a : Nat} -> {b : Nat} -> {c : Nat} -> {d : Nat} ->
               (a = b) -> (c = d) -> (a + c = b + d)
    reflPlusRefl eq1 eq2 = rewrite eq1 in rewrite eq2 in Refl

IntSetoid : Setoid
IntSetoid = MkSetoid MyInt IntEq $ EqProof IntEq intRefl intSym intTrans
