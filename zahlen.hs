module Zahlen
(
    numberToNummer,
    allesNummer
)
where


allesNummer = map numberToNummer [1..]

numberToNummer :: Integer -> String
numberToNummer i
    | i > 999 = (numberOverThousand i)
    | i > 99 = (numberOverHundred i)
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
    | i == 12 = "Zwolf"
    | i < 20 = (numberUnderTwenty i)
    | i == 20 = "Zwanzig"
    | i == 30 = "Dreissig"
    | i == 40 = "Vierzig"
    | i == 50 = "Funfzig"
    | i == 60 = "Sechzig"
    | i == 70 = "Siebzig"
    | i == 80 = "Achtzig"
    | i == 90 = "Neunzig"
    | i > 20 = (numberOverTwenty i)
    | otherwise = "Was?"
    where readInt i = read i :: Integer 
          takeLastDigits n i =  readInt . reverse . take n . reverse $ show i
          dropLastDigits n i = readInt . reverse . drop n . reverse $ show i 
          decade i = i - (i `mod` 10)
          numberUnderTwenty i = numberToNummer (takeLastDigits 1 i) ++ "zehn"
          numberOverTwenty i = numberToNummer (takeLastDigits 1 i) ++ "und" ++ numberToNummer (decade i)
          numberOverHundred i = numberToNummer (dropLastDigits 2 i) ++ "hundert" ++ numberToNummer (takeLastDigits 2 i) 
          numberOverThousand i = numberToNummer (dropLastDigits 3 i) ++ "tausend" ++ numberToNummer (takeLastDigits 3 i) 
