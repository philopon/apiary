-- | reexporting Network.Wai to reduce dependencies from apiary-* packages.
module Web.Apiary.Wai
    ( module Network.Wai
    ) where

import Network.Wai
