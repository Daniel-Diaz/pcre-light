import Text.Regex.PCRE.Light.Char8

main = do
    let r = compile "(a|)*\\d" []
    print (match r "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" [])
