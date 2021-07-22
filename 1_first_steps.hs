module Main where
import System.Environment

main :: IO ()
main = do askName

cmdLine = do args <- getArgs
             let a = read $ args !! 0 :: Double
             let b = read $ args !! 1 :: Int
             putStrLn $ args !! 0 ++ " + " ++ args !! 1 ++ " = " ++ show ( a / fromIntegral b)

askName = do putStrLn "What is your name ?"
             line <- getLine
             putStrLn $ "Your name is " ++ line
