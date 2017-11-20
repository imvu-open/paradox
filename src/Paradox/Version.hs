{-# LANGUAGE TemplateHaskell #-}
module Paradox.Version
( versionString
) where

import Options.Applicative.Simple           (simpleVersion)
import qualified Paths_paradox              as Meta

versionString :: String
versionString = $(simpleVersion Meta.version)
