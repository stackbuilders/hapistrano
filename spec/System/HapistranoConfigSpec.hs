{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.HapistranoConfigSpec
  ( spec
  ) where

import qualified Data.Aeson               as A
import           System.Hapistrano.Commands (mkGenericCommand)
import           System.Hapistrano.Config ( Config (..), Target (..)
                                          , BuildCommand (..), ExecutionMode (..)
                                          )
import           System.Hapistrano.Types  (Shell (..), Source (..),
                                           TargetSystem (..))

import qualified Data.Yaml.Config         as Yaml
#if MIN_VERSION_base(4,15,0)
import           Path                     (mkAbsDir, mkRelDir, mkRelFile)
#else
import           Path                     (Abs, Dir, File, Rel, mkAbsDir,
                                           mkRelDir, mkRelFile)
#endif
import           Test.Hspec


spec :: Spec
spec =
  describe "Hapistrano's configuration file" $ do
    describe "BuildCommand" $ do
      fit "" $ do
        let Just cmd = mkGenericCommand "ls"
        A.eitherDecode "\"ls\"" `shouldBe` Right (BuildCommand cmd AllTargets)

      fit "" $ do
        let Just cmd = mkGenericCommand "ls"
        A.eitherDecode "{\"command\":\"ls\"}" `shouldBe` Right (BuildCommand cmd AllTargets)

      fit "" $ do
        let Just cmd = mkGenericCommand "ls"
        A.eitherDecode "{\"command\":\"ls\",\"only_lead\":true}" `shouldBe` Right (BuildCommand cmd LeadTarget)

      fit "" $ do
        let Just cmd = mkGenericCommand "ls"
        A.eitherDecode "{\"command\":\"ls\",\"only_lead\":false}" `shouldBe` Right (BuildCommand cmd AllTargets)

    context "when the key 'local-repository' is present" $
      it "loads LocalRepository as the configuration's source" $
        Yaml.loadYamlSettings ["fixtures/local_directory_config.yaml"] [] Yaml.useEnv
          >>=
            (`shouldBe`
              (defaultConfiguration
              { configSource = LocalDirectory { localDirectoryPath = $(mkAbsDir "/") } }
              )
            )

    context "when the keys 'repo' and 'revision' are present" $
      it "loads GitRepository as the configuration's source" $
        Yaml.loadYamlSettings ["fixtures/git_repository_config.yaml"] [] Yaml.useEnv
          >>= (`shouldBe` defaultConfiguration)

    describe "Config From JSON instances" $ do
      context "when contains unique targets" $
        it "parses all unique targets" $
          let eiHosts = fmap targetHost <$> configHosts <$> A.eitherDecode content
              content =
                "{\"targets\":[{\"host\":\"user@app1.com\"},{\"host\":\"user@app2.com\"}],\"deploy_path\":\"/tmp\",\"local_directory\":\"/\"}"
          in eiHosts `shouldBe` Right ["user@app1.com", "user@app2.com"]

      context "when contains duplicated targets" $
        it "parses all only unique targets" $
          let eiHosts = fmap targetHost <$> configHosts <$> A.eitherDecode content
              content =
                "{\"targets\":[{\"host\":\"user@app1.com\"},{\"host\":\"user@app1.com\"}],\"deploy_path\":\"/tmp\",\"local_directory\":\"/\"}"
          in eiHosts `shouldBe` Right ["user@app1.com"]


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

    , configSource = GitRepository "my-repo" "my-revision"
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
    , configKeepOneFailed = False
    , configWorkingDir = Nothing
    , configMaintenanceDirectory = $(mkRelDir "maintenance")
    , configMaintenanceFileName = $(mkRelFile "maintenance.html")
    }
