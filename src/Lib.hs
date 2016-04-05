{-# LANGUAGE DeriveFunctor, FlexibleInstances, GADTs, InstanceSigs, LambdaCase, Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax                                            #-}

module Lib (someFunc) where

import Control.Monad.Free
import Control.Monad.Morph
import Control.Monad.Operational
import Control.Monad.Trans
import Control.Monad.Trans.Writer.Lazy
import Data.Function                   ((&))
import Prelude
import System.Exit                     hiding (ExitSuccess)


-- Logging Effect --------------------------------------------------------------------------------------------

type LogsT e m a = WriterT [e] m a

logEv :: Monad m => e → LogsT e m ()
logEv ev = tell [ev]

runLogsT :: (Show e, MonadIO m) ⇒ LogsT e m a → m a
runLogsT action = do
    (x, logs) <- runWriterT action
    liftIO (print logs) -- Note that this is just a hack to get something working, we wouldn't
                        -- actually use `WriterT` for this, so we can have logging interspersed.
    return x


-- DSL -------------------------------------------------------------------------------------------------------

data TeletypeF x
    = PutStrLn String x
    | GetLine (String → x)
    | ExitSuccess x
  deriving (Functor)

type TT = Free TeletypeF

putStrLn' :: String → TT ()
putStrLn' str = liftF (PutStrLn str ())

getLine' :: TT String
getLine' = liftF (GetLine id)

exitSuccess' :: TT ()
exitSuccess' = liftF (ExitSuccess ())


-- DSL Logging -----------------------------------------------------------------------------------------------

data TTEvent
    = WroteString String
    | GotLine String
    | Done
    | ExplicitExit
  deriving (Show)

logTeletype :: TT a → LogsT TTEvent TT a
logTeletype tt = do
    x <- run tt
    logEv Done
    return x
  where
    run :: TT a → LogsT TTEvent TT a
    run = \case
        Pure r → return r
        Free x → x & \case
            PutStrLn ln k → do
                lift (putStrLn' ln)
                logEv (WroteString ln)
                run k
            GetLine k → do
                ln <- lift getLine'
                logEv (GotLine ln)
                (run . k) ln
            ExitSuccess k → do
                logEv ExplicitExit
                lift exitSuccess'
                run k


-- Interpreter -----------------------------------------------------------------------------------------------

runTT :: TT a → IO a
runTT tt = case tt of
    Pure r      → return r
    Free action → case action of
        PutStrLn str k → putStrLn str >> runTT k
        GetLine k      → getLine      >>= runTT . k
        ExitSuccess k  → return ()    >> runTT k -- TODO


runTTWithLogging :: ∀a. TT a → LogsT TTEvent IO a
runTTWithLogging tt =
    let x :: LogsT TTEvent TT a = logTeletype tt
    in hoist runTT x


-- Example ---------------------------------------------------------------------------------------------------

exampleProgram :: TT ()
exampleProgram = do
    ln <- getLine'
    putStrLn' ("Got a line: " ++ ln)
    ln <- getLine'
    putStrLn' ("Got another line: " ++ ln)
    exitSuccess'

someFunc = runLogsT (runTTWithLogging exampleProgram)
