{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Apiary.TH where

import Control.Monad.Apiary
import Control.Monad.Apiary.Action
import Network.HTTP.Types.Status
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S
import Network.Mime

numToCode :: Int -> ExpQ
numToCode = \case
    100 -> varE 'status100
    101 -> varE 'status101
    200 -> varE 'status200
    201 -> varE 'status201
    n   -> fail $ "unknown HTTP status code:" ++ show n

-- | shortcut action. since 0.5.2.0.
--
-- @
-- [act|200 .html|] == [act|200 text/html|] ==
-- action $ \arguments -> do
--     status 200
--     contentType "text/html"
-- @
act :: QuasiQuoter
act = QuasiQuoter 
    { quoteExp  = act'
    , quotePat  = \_ -> fail "act QQ only Exp."
    , quoteType = \_ -> fail "act QQ only Exp."
    , quoteDec  = \_ -> fail "act QQ only Exp."
    }

parseAct :: String -> (Int, String)
parseAct s =
    let (code, ct) = T.break (== ' ') . T.strip $ T.pack s
        mime       = case T.strip ct of
            t | T.head t == '.' -> defaultMimeLookup t
              | otherwise       -> S.pack $ T.unpack t
    in (read $ T.unpack code, S.unpack mime)

act' :: String -> ExpQ
act' s = 
    let (code, mime) = parseAct s
         in [| actionWithPreAction (\_ -> do 
             status $(numToCode code)
             contentType $(stringE mime)
             )|]
