module Zahlen
(
    numberToNummer
)
where

numberToNummer :: Integer -> String
numberToNummer i
    | i == 0 = "Null"
    | i == 1 = "Eins"
    | i == 2 = "Zwei"
    | i == 3 = "Drei"
    | i == 4 = "Vier"
    | i == 5 = "Funf"
    | i == 6 = "Sechs"
    | i == 7 = "Sieben"
    | i == 8 = "Acht"
    | i == 9 = "Neun"
    | i == 10 = "Zehn"
    | i == 11 = "Elf"
    | i == 12 = "Zwelf"
    | i < 20 = (lastDigit i) ++ "zehn"
    | i == 20 = "Zwanzig"
    | i == 30 = "Dreissig"
    | i == 40 = "Vierzig"
    | i == 50 = "Funfzig"
    | i == 60 = "Sechzig"
    | i == 70 = "Siebzig"
    | i == 80 = "Achtzig"
    | i == 90 = "Neunzig"
    | i > 20 = (numbersOverTwenty i)
    | otherwise = "Was?"
    where lastDigit l = numberToNummer (l `mod` 10)
          decade i = i - (i `mod` 10)
          numbersOverTwenty i = (lastDigit i) ++ "und" ++ numberToNummer (decade i)
