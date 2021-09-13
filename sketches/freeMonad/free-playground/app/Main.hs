{-# LANGUAGE DeriveFunctor #-}
module Main where

import Lib
import Control.Monad.Free

data InterfaceF a =
    WriteResponse String a
    | GetInput (String -> a)
    deriving (Functor)

type Interface = Free InterfaceF
main :: IO ()
main = entryPoint
