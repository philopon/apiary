{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Web.Apiary
import Network.Wai.Handler.Warp
import qualified Data.Text as T

#if MIN_VERSION_apiary(0,15,2)
import Control.Monad
#define SIMPLE(r) [capture|/deep/foo/bar/baz/r|] . method GET . action $ bytes "deep"
#else
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
#define SIMPLE(r) [capture|/deep/foo/bar/baz/r|] . method GET . action $ lbs "deep"
#endif

main :: IO ()
main = do
    port:_ <- getArgs
    run (read port) . runApiary def $ do
        [capture|/echo/hello-world|] . method GET . action $
#if MIN_VERSION_apiary(0,15,2)
             bytes "Hello World"
#else
             lbs "Hello World"
#endif

        [capture|/echo/plain/:T.Text/:Int|] . method GET . action $ \s i ->
#if MIN_VERSION_apiary(0,15,2)
             replicateM_ i (text s)
#else
             lbs . TL.encodeUtf8 . TL.fromChunks $ replicate i s
#endif

        SIMPLE(0)
        SIMPLE(1)
        SIMPLE(2)
        SIMPLE(3)
        SIMPLE(4)
        SIMPLE(5)
        SIMPLE(6)
        SIMPLE(7)
        SIMPLE(8)
        SIMPLE(9)
        SIMPLE(10)
        SIMPLE(11)
        SIMPLE(12)
        SIMPLE(13)
        SIMPLE(14)
        SIMPLE(15)
        SIMPLE(16)
        SIMPLE(17)
        SIMPLE(18)
        SIMPLE(19)
        SIMPLE(20)
        SIMPLE(21)
        SIMPLE(22)
        SIMPLE(23)
        SIMPLE(24)
        SIMPLE(25)
        SIMPLE(26)
        SIMPLE(27)
        SIMPLE(28)
        SIMPLE(29)
        SIMPLE(30)
        SIMPLE(31)
        SIMPLE(32)
        SIMPLE(33)
        SIMPLE(34)
        SIMPLE(35)
        SIMPLE(36)
        SIMPLE(37)
        SIMPLE(38)
        SIMPLE(39)
        SIMPLE(40)
        SIMPLE(41)
        SIMPLE(42)
        SIMPLE(43)
        SIMPLE(44)
        SIMPLE(45)
        SIMPLE(46)
        SIMPLE(47)
        SIMPLE(48)
        SIMPLE(49)
        SIMPLE(50)
        SIMPLE(51)
        SIMPLE(52)
        SIMPLE(53)
        SIMPLE(54)
        SIMPLE(55)
        SIMPLE(56)
        SIMPLE(57)
        SIMPLE(58)
        SIMPLE(59)
        SIMPLE(60)
        SIMPLE(61)
        SIMPLE(62)
        SIMPLE(63)
        SIMPLE(64)
        SIMPLE(65)
        SIMPLE(66)
        SIMPLE(67)
        SIMPLE(68)
        SIMPLE(69)
        SIMPLE(70)
        SIMPLE(71)
        SIMPLE(72)
        SIMPLE(73)
        SIMPLE(74)
        SIMPLE(75)
        SIMPLE(76)
        SIMPLE(77)
        SIMPLE(78)
        SIMPLE(79)
        SIMPLE(80)
        SIMPLE(81)
        SIMPLE(82)
        SIMPLE(83)
        SIMPLE(84)
        SIMPLE(85)
        SIMPLE(86)
        SIMPLE(87)
        SIMPLE(88)
        SIMPLE(89)
        SIMPLE(90)
        SIMPLE(91)
        SIMPLE(92)
        SIMPLE(93)
        SIMPLE(94)
        SIMPLE(95)
        SIMPLE(96)
        SIMPLE(97)
        SIMPLE(98)
        SIMPLE(99)
        SIMPLE(100)

        [capture|/after|] . method GET . action $
#if MIN_VERSION_apiary(0,15,2)
            bytes "after"
#else
            lbs "after"
#endif
