import Zahlen

main = do
    putStrLn "Give us an english number is: "
    number <- getLine
    let n = read number :: Integer
    putStrLn $ "Im deutsch" ++ numberToNummer(n)

