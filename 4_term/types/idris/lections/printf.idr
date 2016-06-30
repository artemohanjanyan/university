data Format
    = FInt Format
    | FString Format
    | FOther Char Format
    | FEnd

format : List Char -> Format
format ('%'::'d'::cs) = FInt $ format cs
format ('%'::'s'::cs) = FString $ format cs
format (c::cs) = FOther c $ format cs
format [] = FEnd

interpFormat : Format -> Type
interpFormat (FInt x) = Int -> interpFormat x
interpFormat (FString x) = String -> interpFormat x
interpFormat (FOther _ x) = interpFormat x
interpFormat FEnd = String

formatString : String -> Format
formatString s = format (unpack s)

toFunction : (fmt : Format) -> String -> interpFormat fmt
toFunction (FInt f) a = \i => toFunction f (a ++ show i)
toFunction (FString f) a = \s => toFunction f (a ++ s)
toFunction (FOther c f) a = toFunction f (a ++ singleton c)
toFunction FEnd a = a

sprintf : (s : String) -> interpFormat (formatString s)
sprintf s = toFunction (formatString s) ""

-- 1) Prove AC in Idris
-- 2) Define setoid MyInt (Int)
-- 3) Define setoid of multiplicative integers modulo n
