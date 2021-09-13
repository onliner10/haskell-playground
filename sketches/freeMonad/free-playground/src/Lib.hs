{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( entryPoint
    ) where

-- import Control.Monad.Free
import Control.Monad.Trans.Free


data InterfaceF next =
    WriteResponse String next
    | GetInput (String -> next)
    deriving (Functor)

type Interface = Free InterfaceF

writeResponse :: String -> Interface ()
writeResponse x = liftF (WriteResponse x ())

getInput :: Interface String
getInput = liftF (GetInput id)

greeter = do
    writeResponse "Start"
    writeResponse "Tell me yr name"
    name <- getInput
    writeResponse ("Hello " ++ name)

interpreter :: Interface a -> IO a
interpreter command = 
    case runFree command of
        Pure x -> return x
        Free (WriteResponse text next) -> do
            putStrLn text
            interpreter next
        Free (GetInput next) -> do
            input <- getLine
            interpreter $ next input

entryPoint = do
    interpreter greeter
    putStrLn "End"
