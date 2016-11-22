module Hap.Shared.Utils where

import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS
import           System.IO (hPutStrLn, stderr)
import           System.Exit (ExitCode(..), exitWith)

errorHandler :: String -> IO ()
errorHandler msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)

readConfiguration :: FromJSON a => FilePath -> IO (Either String a)
readConfiguration f = fmap eitherDecode $ BS.readFile f
