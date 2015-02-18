{-# LANGUAGE TemplateHaskell #-}
module Web.Apiary.TH(key) where

import Data.Apiary.SProxy(SProxy(..))
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH

-- | construct string literal proxy.
--
-- prop> [key|foo|] == (Proxy :: Proxy "foo")
--
key :: QuasiQuoter
key = QuasiQuoter
    { quoteExp  = \s -> [| SProxy :: SProxy $(TH.litT $ TH.strTyLit s) |]
    , quotePat  = error "key qq only exp or type."
    , quoteType = \s -> [t| SProxy $(TH.litT $ TH.strTyLit s) |]
    , quoteDec  = error "key qq only exp or type."
    }
