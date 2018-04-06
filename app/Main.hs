{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Control.Lens               ((^.))
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Conduit.Binary        (sinkLbs)
import           Data.Either                (Either (Left, Right))
import           Data.Text                  (Text, pack, unpack)
import           Network.AWS                (AccessKey, Credentials (FromKeys),
                                             Region (Tokyo), SecretKey, newEnv,
                                             runAWS, runResourceT, send,
                                             sinkBody, within)
import           Network.AWS.Data           (fromText)
import           Network.AWS.S3             (BucketName (BucketName),
                                             ObjectKey (ObjectKey), getObject,
                                             gorsBody)
import           Prelude                    (IO, String, error, pure, putStrLn,
                                             ($), (++), (.), (<$>))
import           System.Environment         (getEnv)

fromKeys' :: Text -> Text -> Either String Credentials
fromKeys' accessKeyIdText secretAccessKeyText = do
  accessKeyId <- fromText accessKeyIdText :: Either String AccessKey
  secretAccessKey <- fromText secretAccessKeyText :: Either String SecretKey
  pure $ FromKeys accessKeyId secretAccessKey

main :: IO ()
main = do
  accessKeyIdText <- pack <$> getEnv "HS3_ACCESS_KEY_ID"
  bucketNameText <- pack <$> getEnv  "HS3_BUCKET_NAME"
  secretAccessKeyText <- pack <$> getEnv  "HS3_SECRET_ACCESS_KEY"
  putStrLn $ "AccessKeyId: " ++ unpack accessKeyIdText
  putStrLn $ "BucketName: " ++ unpack bucketNameText
  putStrLn $ "SecretAccessKey: " ++ unpack secretAccessKeyText
  let objectKey = ObjectKey "hello.txt"
  let bucketName = BucketName bucketNameText
  env <- case fromKeys' accessKeyIdText secretAccessKeyText of
    (Left _)            -> error "invalid credentials"
    (Right credentials) -> newEnv credentials
  let request = getObject bucketName objectKey
  lbs <- runResourceT . runAWS env $ within Tokyo $ do
    response <- send request
    sinkBody (response ^. gorsBody) sinkLbs
  putStrLn $ B.unpack lbs
