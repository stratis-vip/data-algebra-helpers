{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_algebra_helpers (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "algebra_helpers"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Helper functions for project \"school algebra\" "
copyright :: String
copyright = ""
homepage :: String
homepage = ""
