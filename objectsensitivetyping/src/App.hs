{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module App where

import Data.Map as M    
import Control.Applicative    
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
    
import AppConfig
import Framework.Solve    
import Analysis.SensitivitySpec    
import PHP.IR

-- Defines monad transformer stack

type AppState = Stats (Label,Context) 

newtype App a = App {  
    -- ErrorT for error messages
    -- ReaderT for application configuration
    -- WriterT for debug/log messages
    -- StateT for fresh variables.
    runApp :: ErrorT String (ReaderT AppConfig (WriterT [String] (StateT AppState Identity))) a
} deriving (Monad, MonadReader AppConfig, MonadWriter [String], MonadError String, MonadState AppState, Applicative, Functor)


runStack config app = (fst . fst)
                    . runIdentity 
                    . flip runStateT (Stats M.empty) 
                    . runWriterT 
                    . flip runReaderT config 
                    . runErrorT 
                    . runApp 
                    $ app
                   