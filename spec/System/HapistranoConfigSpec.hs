{-# LANGUAGE TemplateHaskell #-}
module System.HapistranoConfigSpec
  ( spec
  ) where

import           System.Hapistrano.Config (Config (..), Target (..))
import           System.Hapistrano.Types  (Shell (..),
                                           Source (..), TargetSystem (..))

import qualified Data.Yaml.Config         as Yaml
import           Path                     (mkAbsDir)
import           Test.Hspec


spec :: Spec
spec =
  describe "Hapistrano's configuration file" $ do
    context "when the key 'local-repository' is present" $
      it "loads LocalRepository as the configuration's source" $
        Yaml.loadYamlSettings ["fixtures/local_directory_config.yaml"] [] Yaml.useEnv
          >>= (`shouldBe` defaultConfiguration)

    context "when the keys 'repo' and 'revision' are present" $
      it "loads GitRepository as the configuration's source" $
        Yaml.loadYamlSettings ["fixtures/git_repository_config.yaml"] [] Yaml.useEnv
          >>= (`shouldBe` (defaultConfiguration { configSource = GitRepository "my-repo" "my-revision" }))


defaultConfiguration :: Config
defaultConfiguration =
  Config
    { configDeployPath = $(mkAbsDir "/")
    , configHosts =
      [ Target
          { targetHost = "www.example.com"
          , targetPort = 22
          , targetShell = Bash
          , targetSshArgs = []
          }
      ]
    , configSource = LocalDirectory { localDirectoryPath = $(mkAbsDir "/") }
    , configRestartCommand = Nothing
    , configBuildScript = Nothing
    , configCopyFiles = []
    , configCopyDirs = []
    , configLinkedFiles = []
    , configLinkedDirs = []
    , configVcAction = True
    , configRunLocally = Nothing
    , configTargetSystem = GNULinux
    , configReleaseFormat = Nothing
    , configKeepReleases = Nothing
    }
