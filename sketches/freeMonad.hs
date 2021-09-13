{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}


import Control.Monad.Free

data InterfaceF next =
    WriteResponse String next
    | GetInput (String -> next)
    deriving (Functor)

type Interface = Free InterfaceF

writeResponse :: String -> Interface
writeResponse x = liftF $ WriteResponse x id

getInput :: (String -> a) -> Interface
getInput f = liftF $ GetInput f

greeter = do
    writeResponse "Start"
    name <- getInput
    writeResponse ("Hello " ++ name)

interpreter :: InterfaceF a -> IO a
interpreter (WriteResponse text next) = do
    purStrLn text
    interpreter $ next text
interpreter (GetInput f) = do 
    input <- getLine
    interpreter $ next $ f input

main =
    interpreter greeter
    putStrLn "End"