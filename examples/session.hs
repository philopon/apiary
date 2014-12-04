{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Web.Apiary
import Web.Apiary.Session.ClientSession
import Network.Wai.Handler.Warp

main :: IO ()
main = runApiaryWith (run 3000) (initClientSession pInt def{csCookieSecure = False, csTTL = Just 10} ) def $ do
    [capture|/|] . action $ do
        liftIO $ putStrLn "/"
        getSession pInt >>= \i -> showing i

    [capture|/set/i::Int|] . action $ do
        i <- param [key|i|]
        setSession pInt i

    [capture|/delete|] . action $ do
        deleteSession pInt

    [capture|/member|] . session pInt . action $ do
        bytes "hello, member id:"
        param [key|session|] >>= appendShowing

-- access                       | message
-- -----------------------------|---------
-- /member                      | 404 Page Notfound.
-- /                            | Nothing
-- /set/12                      |
-- /                            | Just 12
-- /member                      | hello, member id:12
-- (10sec left without access)  |
-- /member                      | 404 Page Notfound.
-- /                            | Nothing
-- /set/20                      |
-- /delete                      |
-- /                            | Nothing
