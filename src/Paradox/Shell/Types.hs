{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Paradox.Shell.Types where

import Data.Aeson           (FromJSON(..))

import qualified Data.Text  as TS

newtype ShellCommand = ShellCommand { unShellCommand :: TS.Text }
                     deriving (Show, Eq, Ord)

newtype ShellArgument = ShellArgument { unShellArgument :: TS.Text }
                      deriving (Show, Eq, Ord)


deriving instance FromJSON ShellCommand
deriving instance FromJSON ShellArgument

data UnsafeSandboxedAction = UnsafeSandboxedAction
    { usaCmd :: TS.Text
    , usaRest :: [TS.Text]
    } deriving (Eq, Ord, Show)

data SafeSandboxedAction = SafeSandboxedAction
    { ssaCmd :: ShellCommand
    , ssaRest :: [ShellArgument]
    } deriving (Eq, Ord, Show)

