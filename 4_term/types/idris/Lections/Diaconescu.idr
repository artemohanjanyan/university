module Lections.Diaconescu

import Setoid

%access public export

||| Чтобы создать экземпляр Or, мы либо создадим a, либо создадим b.
data Or a b = Inl a | Inr b

data Map : (A:Setoid) -> (B:Setoid) -> Type where
  MkMap: {A:Setoid} -> {B:Setoid} -> (f: (Carrier A) -> (Carrier B)) ->
  ({x:Carrier A} -> {y:Carrier A} -> ((Equiv A) x y) -> ((Equiv B) (f x) (f y))) -> Map A B

-- Мы не смогли в record для зависимых типов
MapF : {A:Setoid} -> {B:Setoid} -> Map A B -> (Carrier A -> Carrier B)
MapF (MkMap {A} {B} f ext) = f
-- На заметку: ((MapF f) x) == (MapF f x)

-- Мы не смогли в record для зависимых типов [2]
MapExt : {A:Setoid} -> {B:Setoid} -> (p: Map A B) ->
     ({x:Carrier A} -> {y:Carrier A} -> ((Equiv A) x y) -> ((Equiv B) (MapF p x) (MapF p y)))
MapExt (MkMap {A} {B} f ext) = ext

-- Нахер не нужно
-- Rel: Type -> Type -> Type
-- Rel a b = a -> b -> Type

-- Вводим аксиому выбора (chs -- функция выбора)
-- (∀x∈I . ∃g∈S . A(x, g)) -> ∃(chs:I->S) . ∀w∈I . A(w, chs(w))
postulate ext_ac : {I : Setoid} -> {S: Setoid} ->
  (A: Carrier I -> Carrier S -> Type) ->
  ((x : Carrier I) -> (g : Carrier S ** A x g)) ->
  (chs : (Map I S) ** ((w : Carrier I) -> A w (MapF chs w)))

-- Закон исключённого третьего: для любого утверждения P строим P v ¬P
excluded_middle : (P: Type) -> Or P (Not P)
excluded_middle P = decide
  where
    -- Интенсиональный сетоид
    Discrete : Setoid
    Discrete = intensional_setoid Bool

    -- Определим "странное" отношение эквивалентности
    strange_eq : Bool -> Bool -> Type
    strange_eq x y = Or (x = y) P

    -- Сетоид над таким отношением
    Strange : Setoid
    Strange = MkSetoid Bool strange_eq (EqProof strange_eq strange_refl strange_sym strange_trans)
      where
        strange_refl : Reflx strange_eq
        strange_refl x = Inl (Refl {x=x})

        strange_sym : Symm strange_eq
        strange_sym x y (Inl r) = Inl $ sym r
        strange_sym x y (Inr r) = Inr r

        strange_trans : Trans strange_eq
        strange_trans x y z (Inl r) (Inl t) = Inl $ trans r t
        strange_trans x y z (Inr r) _       = Inr r
        strange_trans x y z _       (Inr t) = Inr t

    -- Дальше я чёт не вник но дописать смог
    extF : (f : Map Strange Discrete ** ((x : Bool) -> strange_eq x (MapF f x)))
    extF = ext_ac {I = Strange} {S = Discrete} strange_eq (\x : Bool => (x ** Inl Refl))

    f : Map Strange Discrete
    f = fst extF

    ext : (x : Bool) -> strange_eq x (MapF f x)
    ext = snd extF

    lemma1 : strange_eq True False -> P
    lemma1 (Inl r) = void $ trueNotFalse r
    lemma1 (Inr q) = q

    lemma2 : (MapF f True) = (MapF f False) -> strange_eq True False
    lemma2 x = case ext False of
        Inr r => Inr r
        Inl l => case ext True of
            Inr r => Inr r
            Inl l' => Inl $ trans (trans l' x) $ sym l

    decide : Or P (Not P)
    decide with (decEq (MapF f True) (MapF f False))
        | No  pr = Inr (\px => pr $ MapExt f $ Inr px)
        | Yes pr = Inl $ lemma1 $ lemma2 pr

main : IO ()
main = putStrLn "works"
