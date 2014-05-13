{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Web.Apiary.Route.Internal where

import Control.Monad
import Network.Wai
import Text.Read
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Int
import Data.Word

import Web.Apiary.Trans.Internal

preCapture :: [Char] -> [T.Text]
preCapture ('/':s) = T.splitOn "/" $ T.pack s
preCapture s       = T.splitOn "/" $ T.pack s

capture :: QuasiQuoter
capture = QuasiQuoter
    { quoteExp  = capture' . preCapture
    , quotePat  = \_ -> error "No quotePat."
    , quoteType = \_ -> error "No quoteType."
    , quoteDec  = \_ -> error "No quoteDec."
    }

class Param a where
  readParam :: T.Text -> Maybe a

instance Param Char where
    readParam s | T.null s  = Nothing
                | otherwise = Just $ T.head s

instance Param Int     where readParam = readMaybe . T.unpack
instance Param Int8    where readParam = readMaybe . T.unpack
instance Param Int16   where readParam = readMaybe . T.unpack
instance Param Int32   where readParam = readMaybe . T.unpack
instance Param Int64   where readParam = readMaybe . T.unpack
instance Param Integer where readParam = readMaybe . T.unpack

instance Param Word   where readParam = readMaybe . T.unpack
instance Param Word8  where readParam = readMaybe . T.unpack
instance Param Word16 where readParam = readMaybe . T.unpack
instance Param Word32 where readParam = readMaybe . T.unpack
instance Param Word64 where readParam = readMaybe . T.unpack

instance Param Double  where readParam = readMaybe . T.unpack
instance Param Float   where readParam = readMaybe . T.unpack

instance Param T.Text where
    readParam = Just

instance Param TL.Text where
    readParam = Just . TL.fromStrict

instance Param String where
    readParam = Just . T.unpack

integralE :: Integral i => i -> ExpQ
integralE = litE . integerL . fromIntegral

capture' :: [T.Text] -> ExpQ
capture' cap = [| function $ \_ request -> 
    $(caseE [|pathInfo request|] 
        [ match pat   (guards >>= \g -> body >>= \b -> normalB (doE $ g ++ b)) []
        , match wildP (normalB  [|mzero|]) []
        ]) |]
  where
    varNames = zip cap $ map (('v':) . show) [0 :: Int ..]
    pat      = listP $ map (varP . mkName . snd) varNames
    isType s | T.null s        = False
             | T.head s == ':' = True
             | otherwise       = False
    guards = return $ 
        map (\(a,v) -> noBindS [|guard $ $(varE $ mkName v) == $(stringE $ T.unpack a) |]) $
        filter (not . isType . fst) varNames
    body = do
        let ss = map (\(a,v) -> do
                ty <- lookupType a
                -- let ty = mkName . T.unpack $ T.tail a
                bindS (varP . mkName $ v ++ "'")
                    [| (readParam $(varE $ mkName v) :: Maybe $(conT ty) ) |])
                    $ filter (isType . fst) varNames
            rt = tupE $ map (\(_,v) -> varE $ mkName (v ++ "'")) $ filter (isType . fst) varNames
        return $ ss ++ [noBindS [| return $rt |]]
    lookupType n =  lookupTypeName (T.unpack $ T.tail n) >>= \case
        Nothing -> fail $ "capture': type not found: " ++ T.unpack (T.tail n)
        Just ty -> return ty


