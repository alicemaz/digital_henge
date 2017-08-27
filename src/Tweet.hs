{-# LANGUAGE OverloadedStrings #-}

module Tweet (tweet) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Web.Twitter.Conduit

mkCreds :: [BC.ByteString] -> TWInfo
mkCreds (key:sec:tok:toksec:[]) = setCredential keys tokens def
    where keys = twitterOAuth { oauthConsumerKey = key, oauthConsumerSecret = sec }
          tokens = Credential [("oauth_token", tok), ("oauth_token_secret", toksec)]
mkCreds _ = undefined

tweet :: String -> IO ()
tweet s = do
    creds <- (mkCreds . BC.lines) <$> BC.readFile ".credentials"
    mgr <- newManager tlsManagerSettings
    _ <- call creds mgr $ update (T.pack s)
    pure ()
