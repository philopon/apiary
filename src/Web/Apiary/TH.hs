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
    202 -> varE 'status202
    203 -> varE 'status203
    204 -> varE 'status204
    205 -> varE 'status205
    206 -> varE 'status206

    300 -> varE 'status300
    301 -> varE 'status301
    302 -> varE 'status302
    303 -> varE 'status303
    304 -> varE 'status304
    305 -> varE 'status305
    307 -> varE 'status307

    400 -> varE 'status400
    401 -> varE 'status401
    402 -> varE 'status402
    403 -> varE 'status403
    404 -> varE 'status404
    405 -> varE 'status405
    406 -> varE 'status406
    407 -> varE 'status407
    408 -> varE 'status408
    409 -> varE 'status409
    410 -> varE 'status410
    411 -> varE 'status411
    412 -> varE 'status412
    413 -> varE 'status413
    414 -> varE 'status414
    415 -> varE 'status415
    416 -> varE 'status416
    417 -> varE 'status417
    418 -> varE 'status418

    500 -> varE 'status500
    501 -> varE 'status501
    502 -> varE 'status502
    503 -> varE 'status503
    504 -> varE 'status504
    505 -> varE 'status505

    n   -> fail $ "unknown HTTP status code:" ++ show n

-- | shortcut action. since 0.6.0.0.
--
-- @
-- [act|200 .html|] == [act|200 text/html|] ==
-- action $ \\arguments -> do
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
