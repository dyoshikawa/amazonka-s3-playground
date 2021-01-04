{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text          as Text
import           Lib
import           System.Environment

main :: IO ()
main = do
    initEnv
    env <- createAwsEnv
    bucketName <- getEnv "AWS_S3_BUCKET_NAME"
    s3Upload env (Text.pack bucketName) "10m.dummy" "./10m.dummy"
    s3StreamUpload env (Text.pack bucketName) "100m.dummy" "./100m.dummy"
    return ()
