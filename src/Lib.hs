{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( initEnv,
      createAwsEnv,
      s3Upload,
      s3StreamUpload
    ) where

import           Conduit
import           Configuration.Dotenv
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.ByteString.Char8                as BC
import           Data.Text
import qualified Data.Text.IO                         as Text
import           Network.AWS.S3
import           Network.AWS.S3.CreateMultipartUpload
import           Network.AWS.S3.StreamingUpload
import           System.Environment
import           System.IO


initEnv :: IO ()
initEnv = void $ loadFile defaultConfig

createAwsEnv :: IO Env
createAwsEnv = do
    logger <- newLogger Debug stdout
    accessKeyId <- getEnv "AWS_ACCESS_KEY_ID"
    secretAccessKey <- getEnv "AWS_SECRET_ACCESS_KEY"
    newEnv (FromKeys (AccessKey $ BC.pack accessKeyId) (SecretKey $ BC.pack secretAccessKey)) <&> set envLogger logger . set envRegion Tokyo

s3Upload :: Env -> Text -> Text -> String -> IO ()
s3Upload env bucketName key filePath =
    runResourceT . runAWST env $ do
        body <- chunkedFile 10000000 filePath
        send $ putObject (BucketName bucketName) (ObjectKey key) body
        return ()

s3StreamUpload :: Env -> Text -> Text -> String -> IO ()
s3StreamUpload env bucketName key filePath =
    runResourceT . runAWST env $ do
        runConduit (sourceFileBS filePath .| streamUpload Nothing (createMultipartUpload (BucketName bucketName) (ObjectKey key)))
        return ()
