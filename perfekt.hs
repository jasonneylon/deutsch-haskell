module Perfekt
(
  Verb (..),
  Pronoun (..),
  toPerfekt,
  toPresent,
  conjugate
)
where

-- http://www.nthuleen.com/teach/grammar/perfektexpl.html

data Verb = WeakVerb String | LatinVerb String | StrongVerb String String | 
            InseperablePrefixVerb String | SeperablePrefixVerb String String |
            MotionVerb String
            deriving Show

data Pronoun = Ich | Du | Er | Sie | Wir | Ihr | SIE

toPerfekt :: Verb -> String
toPerfekt (WeakVerb v) = "Ich habe ge" ++ (dropLastTwoLetters v) ++ "t"
toPerfekt (MotionVerb v) = "Ich bin ge" ++ v 
toPerfekt (LatinVerb v) = "Ich habe " ++ (dropLastTwoLetters v) ++ "t"
toPerfekt (InseperablePrefixVerb v) = "Ich habe " ++ (dropLastTwoLetters v) ++ "t"
toPerfekt (SeperablePrefixVerb v prefix) = "Ich habe " ++ prefix ++ "ge" ++ (dropLastTwoLetters (endOfSeperableVerb v prefix)) ++ "t"
toPerfekt (StrongVerb _ p) = "Ich habe ge" ++ p

toPresent :: Verb -> String
toPresent (SeperablePrefixVerb v prefix) = "Ich " ++ (conjugate Ich (endOfSeperableVerb v prefix)) ++ " " ++ prefix 
toPresent (WeakVerb v)  =  defaultPresent v
toPresent (MotionVerb v)  = defaultPresent v
toPresent (LatinVerb v)  = defaultPresent v
toPresent (InseperablePrefixVerb v)  = defaultPresent v
toPresent (StrongVerb v _)  = defaultPresent v

defaultPresent :: String -> String
defaultPresent v = "Ich " ++ conjugate Ich v

conjugate :: Pronoun -> String -> String
conjugate Ich v = reverse $ drop 1 $ reverse $ v

endOfSeperableVerb :: String -> String -> String
endOfSeperableVerb v prefix = drop (length prefix) v

dropLastTwoLetters :: String -> String
dropLastTwoLetters s = reverse $ drop 2 $ reverse $ s

verbs :: [Verb]
verbs = [WeakVerb "spielen", WeakVerb "wohnen", WeakVerb "machen", WeakVerb "fragen" 
        , LatinVerb "reparieren", LatinVerb "studieren", LatinVerb "manipulieren"
        , StrongVerb "singen" "sungen", StrongVerb "fliegen" "flogen", StrongVerb "geben" "gegeben"
        , InseperablePrefixVerb "besuchen", InseperablePrefixVerb "erleben", InseperablePrefixVerb "verkaufen" 
        , SeperablePrefixVerb "zumachen" "zu", SeperablePrefixVerb "anrufen" "an"
        , MotionVerb "kommen", MotionVerb "gehen", MotionVerb "wandern", MotionVerb "sterben"]
                            
allTheVerbs :: [String]
allTheVerbs = map (\v -> toPresent v ++ " - " ++ toPerfekt v) verbs

makePrintableString :: [String] -> String
makePrintableString strs = (foldl (\acc v -> acc ++ v ++ "\n" ) "" strs)


