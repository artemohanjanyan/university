other = "other = $\n\nreplace \"\" newStr = \"\"\nreplace (c:s) newStr\n    | c == '$'  = newStr ++ s\n    | otherwise = c:(replace s newStr)\n\nsrc = replace other (show other)\n\nmain = do\n    let y = src\n    putStr y\n"

replace "" newStr = ""
replace (c:s) newStr
    | c == '$'  = newStr ++ s
    | otherwise = c:(replace s newStr)

src = replace other (show other)

main = do
    let y = src
    putStr y
