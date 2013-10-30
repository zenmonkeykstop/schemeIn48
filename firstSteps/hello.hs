module Main where
import System.Environment

main :: IO ()
main = do putStrLn ("Hey there, what's your name?")
          theName <- getLine
          putStrLn ("Hello, " ++ theName ++ ", I'm so uninterested in seeing you!")
