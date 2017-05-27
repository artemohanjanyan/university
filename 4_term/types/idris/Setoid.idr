module Setoid

%access public export
%default total

||| Reflexivity property
Reflx : {A : Type} -> (R : A -> A -> Type) -> Type
Reflx {A} R = (x : A) -> R x x

||| Symmetry property
Sym : {A : Type} -> (R : A -> A -> Type) -> Type
Sym {A} R = (x : A) -> (y : A) -> R x y -> R y x

||| Transitivity property
Trans : {A : Type} -> (R : A -> A -> Type) -> Type
Trans {A} R = (x : A) -> (y : A) -> (z : A) -> R x y -> R y z -> R x z

||| Equivalence relation
|||
||| Reflexive, symmetric and transitive relation is equivalence relation
data IsEquivalence : {A : Type} -> (R : A -> A -> Type) -> Type where
    ||| Proof is constructed with proofs of three properties
    EqProof : {A : Type} -> (R : A -> A -> Type) -> Reflx {A} R -> Sym {A} R -> Trans {A} R -> IsEquivalence {A} R

||| Setoid is a set with equivalence relation
record Setoid where
    constructor MkSetoid
    ||| Type of setoid
    Carrier : Type
    ||| Equivalence relation
    Equiv : Carrier -> Carrier -> Type
    ||| Proof that Equiv is equivalence relation
    EquivProof : IsEquivalence Equiv

||| Constructs setoid based on (=) relation
intensional_setoid : Type -> Setoid
intensional_setoid t = MkSetoid t (=) (e {Carrier=t})
  where
    refl_eq : {Carrier : Type} -> Reflx {A=Carrier} (=)
    refl_eq x = Refl {x=x}

    sym_eq : {Carrier : Type} -> Sym {A = Carrier} (=)
    sym_eq x y = sym

    trans_eq : {Carrier : Type} -> Trans {A = Carrier} (=)
    trans_eq a b c = trans

    e : {Carrier : Type} -> IsEquivalence {A=Carrier} (=)
    e {Carrier} = EqProof {A=Carrier} (=) refl_eq sym_eq trans_eq
