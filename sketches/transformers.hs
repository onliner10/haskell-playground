
import Control.Monad.Reader
import Control.Monad.Trans.Writer
import GHC.Base (liftA)

newtype GreetingLabel = GreetingLabel String
data RuntimeEnv = RuntimeEnv { greeting :: GreetingLabel, someDependency :: String, yetAnotherDependency :: String }

defaultRuntimeEnv = RuntimeEnv { greeting = GreetingLabel "Helloooo ", someDependency = "compile please", yetAnotherDependency = "wat" }

doGreeting :: WriterT [String] (ReaderT GreetingLabel IO) ()
doGreeting = do
    tell ["Asking for a name.."]
    liftIO $ putStrLn "Tell me your name"
    GreetingLabel greetingLabel <- ask

    tell ["Got name: " ++ greetingLabel]

    name <- liftIO getLine

    tell ["Composing response"]
    liftIO $ putStrLn $ greetingLabel ++ name

main = do
    -- runReaderT doGreeting $ greeting defaultRuntimeEnv

    ((), logs) <- runReaderT (runWriterT doGreeting) (greeting defaultRuntimeEnv)

    putStrLn "---- Logs ----"
    mapM_ putStrLn logs

    putStrLn  "End"