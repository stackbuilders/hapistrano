{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Hapistrano.Internal (initConfig') where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import qualified Data.Yaml                  as Yaml

import           System.IO                  (stderr)
import           System.Exit                (exitFailure)
import qualified System.FilePath            as FilePath
import qualified System.Directory           as Directory

import           System.Hapistrano.Types (InitTemplateConfig(..), defaultInitTemplateConfig)


-- | Create a file with an initial config file by getting information from the
-- user.
initConfig' :: forall m. MonadIO m => (String -> m String) -> m ()
initConfig' getLine' = do
  configFilePath <- (FilePath.</> "hap.yml") <$> liftIO Directory.getCurrentDirectory
  alreadyExisting <- liftIO $ Directory.doesFileExist configFilePath
  when alreadyExisting $ liftIO $ do
    T.hPutStrLn stderr "'hap.yml' already exists"
    exitFailure
  liftIO $ putStrLn "Creating 'hap.yml'"
  defaults <- liftIO defaultInitTemplateConfig
  let prompt :: Read a => String -> a -> m a
      prompt title d = do
        liftIO $ T.putStrLn $ T.pack $ title <> "?: "
        x <- getLine' title
        liftIO $ putStrLn $ "Value of X: " <> x
        return $
          if null x
            then d
            else read x
      prompt' :: Read a => String -> (InitTemplateConfig -> T.Text) -> (InitTemplateConfig -> a) -> m a
      prompt' title f fd = prompt title (fd defaults)

  let yesNo :: a -> a -> T.Text -> a
      yesNo t f x = if x == "y" then t else f

  config <-
    InitTemplateConfig
      <$> prompt' "repo" repo repo
      <*> prompt' "revision" revision revision
      <*> prompt' "host" host host
      <*> prompt' "port" (T.pack . show . port) port
      <*> return (buildScript defaults)
      <*> fmap (yesNo (restartCommand defaults) Nothing) (prompt' "Include restart command" (const "Y/n") (const "y"))

  liftIO $ do
    Yaml.encodeFile configFilePath config
    putStrLn $ "Configuration written at " <> configFilePath
