{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module System.HapistranoConfigSpec
  ( spec
  ) where

import           System.Hapistrano.Config (Config (..), Target (..))
import           System.Hapistrano.Types  (Shell (..),
                                           Source (..), TargetSystem (..))

import qualified Data.Yaml.Config         as Yaml
#if MIN_VERSION_base(4,15,0)
import           Path                     (mkAbsDir, mkRelDir, mkRelFile)
#else
import           Path                     (mkAbsDir, mkRelDir, mkRelFile, Abs, Rel, Dir, File)
#endif
import           Test.Hspec


spec :: Spec
spec =
  describe "Hapistrano's configuration file" $ do
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
    , configMaintenanceFilePath = $(mkRelDir "maintenance")
    , configMaintenanceFileName = $(mkRelFile "maintenance.html")
    }
