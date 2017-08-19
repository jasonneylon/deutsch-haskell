module Perfekt
(
  Verb (..),
  Pronoun (..),
  toPerfekt,
  toPresent,
  conjugate,
  printSummary,
  verbs,
  allTheVerbsInPerfekt,
  joinStrs
)
where

import Control.Monad.Writer.Lazy

-- http://www.nthuleen.com/teach/grammar/perfektexpl.html

data Verb = WeakVerb String | LatinVerb String | StrongVerb String String | 
            InseperablePrefixVerb String | SeperablePrefixVerb String String |
            MotionVerb String
            deriving Show

data Pronoun = Ich | Du | Er | Sie | Wir | Ihr | SIE

verbs :: [Verb]
verbs = [WeakVerb "spielen", WeakVerb "wohnen", WeakVerb "machen", WeakVerb "fragen" 
        , LatinVerb "reparieren", LatinVerb "studieren", LatinVerb "manipulieren"
        , StrongVerb "singen" "sungen", StrongVerb "fliegen" "flogen", StrongVerb "geben" "gegeben"
        , InseperablePrefixVerb "besuchen", InseperablePrefixVerb "erleben", InseperablePrefixVerb "verkaufen" 
        , SeperablePrefixVerb "zumachen" "zu", SeperablePrefixVerb "anrufen" "an"
        , MotionVerb "kommen", MotionVerb "gehen", MotionVerb "wandern", MotionVerb "sterben"]
 

type SentanceWithReason = Writer String String

toPerfekt :: Verb -> SentanceWithReason 
toPerfekt (WeakVerb v) = do
  tell "Weak verb have ge prepended and their ending changed to t"
  return ("Ich habe ge" ++ (dropLastTwoLetters v) ++ "t")
toPerfekt (MotionVerb v) = do
  tell "Verbs associate with motion use bin and have ge prepended"
  return ("Ich bin ge" ++ v)
toPerfekt (LatinVerb v) = do 
  tell "Latin verbs (ending in -iern have their ending changed to t"
  return ("Ich habe " ++ (dropLastTwoLetters v) ++ "t")
toPerfekt (InseperablePrefixVerb v) = do 
  tell "Verbs with inseperable prefixs have their ending changed to t"
  return ("Ich habe " ++ (dropLastTwoLetters v) ++ "t")
toPerfekt (SeperablePrefixVerb v prefix) = do
  tell "Verbs with seperable prefixs have ge added after the prefix and their ending changed to t"
  return ("Ich habe " ++ prefix ++ "ge" ++ (dropLastTwoLetters (endOfSeperableVerb v prefix)) ++ "t")
toPerfekt (StrongVerb _ p) = do 
  tell "Strong verbs are unchanged"
  return ("Ich habe ge" ++ p)

toPresent :: Verb -> SentanceWithReason
toPresent (SeperablePrefixVerb v prefix) = do 
  tell "In the present tense seperable the prefix of the seperable verb is put at the end of the sentance"
  return ("Ich " ++ (conjugate Ich (endOfSeperableVerb v prefix)) ++ " " ++ prefix)
toPresent (WeakVerb v)  =  defaultPresent v
toPresent (MotionVerb v)  = defaultPresent v
toPresent (LatinVerb v)  = defaultPresent v
toPresent (InseperablePrefixVerb v)  = defaultPresent v
toPresent (StrongVerb v _)  = defaultPresent v

defaultPresent :: String -> SentanceWithReason
defaultPresent v = do 
  tell "In present tense we conjugate the verb appropriately"
  return ("Ich " ++ conjugate Ich v)

conjugate :: Pronoun -> String -> String
conjugate Ich v = reverse $ drop 1 $ reverse $ v

endOfSeperableVerb :: String -> String -> String
endOfSeperableVerb v prefix = drop (length prefix) v

dropLastTwoLetters :: String -> String
dropLastTwoLetters s = reverse $ drop 2 $ reverse $ s


buildExampleLine :: Verb -> SentanceWithReason
buildExampleLine v = do 
  pres <- toPresent v
  perf <- toPerfekt v
  return (pres ++ "--" ++ perf)

allTheVerbsCombined :: [SentanceWithReason]
allTheVerbsCombined = foldl (\acc (l,r) -> acc ++ [l, r]) [] pastPresentPairs
  where pastPresentPairs = zip (map toPresent verbs) (map toPerfekt verbs)

allTheVerbsInPerfekt :: [SentanceWithReason]
allTheVerbsInPerfekt = map toPerfekt verbs

printSummary :: (String, String) -> String
printSummary (s, r) = s ++ " - (" ++ r ++ ")" 

joinStrs :: [String] -> String
joinStrs = foldl (\acc v -> acc ++ v ++ "\n" ) "" 
