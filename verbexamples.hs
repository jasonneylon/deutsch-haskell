import Perfekt 
import Control.Monad.Writer.Lazy

main :: IO ()
main = do
    putStrLn "Past perfekt verb list"
    putStrLn (joinStrs (map (printSummary . runWriter) allTheVerbsInPerfekt))
    -- bigSentanceStr <- foldl (\acc s -> acc ++ s ++ "\n" ) "" sentances
