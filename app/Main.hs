{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Lib                (someFunc)
import           Prelude            (IO, putStrLn, ($), (++))
import           System.Environment (getEnv)

main :: IO ()
main = do
  accessKeyToken <- getEnv "HS3_ACCESS_KEY_ID"
  bucketName <- getEnv  "HS3_BUCKET_NAME"
  secretAccessKey <- getEnv  "HS3_SECRET_ACCESS_KEY"
  putStrLn $ "AccessKeyId: " ++ accessKeyToken
  putStrLn $ "BucketName: " ++ bucketName
  putStrLn $ "SecretAccessKey: " ++ secretAccessKey
