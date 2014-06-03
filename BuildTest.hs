module Main where

import qualified Hapistrano as Hap
import Control.Monad (void)

testConfig :: Hap.Config
testConfig = Hap.Config { Hap._deployPath = "/tmp/project"
                        , Hap._host       = "localhost"
                        , Hap._repository = "/tmp/testrepo"
                        , Hap._revision    = "origin/transformer-refactor"
                        }

rollback :: IO ()
rollback = do
  Hap.runRC errorHandler successHandler (Hap.initialState testConfig) $
    void Hap.rollback

  where
    errorHandler   = Hap.defaultErrorHandler
    successHandler = Hap.defaultSuccessHandler

main :: IO ()
main = do
  Hap.runRC errorHandler successHandler (Hap.initialState testConfig) $
    void $ Hap.pushRelease >> Hap.defaultBuildRelease >> Hap.activateRelease

  where
    errorHandler   = Hap.defaultErrorHandler
    successHandler = Hap.defaultSuccessHandler
