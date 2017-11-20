{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Paradox.Istatd.AsyncClient where

import Prelude hiding (lines)
import Imvu.Network.IstatdClient
import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Concurrent.STM (atomically)
import Control.Exception (try, SomeException)
import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import qualified Network.Socket as Net
import qualified Network.BSD as NetBsd
import qualified Data.Attoparsec.ByteString as Atto
import Data.Attoparsec.ByteString.Char8 (char, endOfLine, isEndOfLine)
import Data.Attoparsec.Combinator (sepBy, skipMany)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Char as Char
import Data.Word (Word8)


data IstatdState = IstatdState {
      queue :: TBMQueue Packet
    , istatdPrefix :: Name
    , istatdSuffixes :: [Name]
    , thread :: Async ()
    }

data IstatdIssue = Counter Name | Gauge Name Double

readCategories :: FilePath -> IO [Name]
readCategories configFile = do
    buffer' <- try $ BS.readFile configFile
    case buffer' of
        Left (_ :: SomeException) -> queryHostname
        Right buffer -> case parse buffer of
            Left _ -> queryHostname
            Right categories -> return categories
    where
        queryHostname :: IO [Name]
        queryHostname = do
            name <- getHostName
            return ["host" <> name]

        getHostName :: IO Name
        getHostName = do
            name <- BSC.pack . map Char.toLower <$> NetBsd.getHostName
            case nameFromBytes name of
                Just x -> return x
                Nothing -> error $
                    "hostname " ++ show name ++ " is not a valid istatd counter name!"


        parse :: BS.ByteString -> Either String [Name]
        parse = Atto.parseOnly istatdCategoriesParser

        istatdCategoriesParser :: Atto.Parser [Name]
        istatdCategoriesParser = lines (skip *> line) <* skip
            where
                skip = skipLines (comment <|> whitespace)
                lines  = flip sepBy endOfLine
                line = do
                    val <- whitespace *> Atto.takeWhile1 (not . isSpaceOrEol) <* whitespace
                    case nameFromBytes val of
                        Just x ->
                            return x
                        Nothing -> fail "Failed to parse categories"

        skipLines :: Atto.Parser a -> Atto.Parser ()
        skipLines parser = skipMany (parser *> endOfLine)

        comment :: Atto.Parser ()
        comment = whitespace *> char '#' *> Atto.skipWhile (not . isEndOfLine)

        isWhitespace,isSpaceOrEol :: Word8 -> Bool
        isWhitespace c = c == 32 || c == 9
        isSpaceOrEol c = isWhitespace c || isEndOfLine c

        whitespace :: Atto.Parser ()
        whitespace = Atto.skipWhile isWhitespace


type Send = IstatdIssue -> IO ()

start :: Net.HostName -> Net.PortNumber -> Name -> String -> IO Send
start host port istatdPrefix categories = do
    queue <- newTBMQueueIO 500
    istatdSuffixes <- readCategories categories
    thread <- do
        let start_ = do
                conn <- connect host port
                async $ runWorker conn queue
        start_

    return $ sendToQueue IstatdState {..}


send_ :: MonadIO m => TBMQueue Packet -> Packet -> m ()
send_ queue = liftIO . void . atomically . writeTBMQueue queue

sendToQueue :: MonadIO m => IstatdState -> IstatdIssue -> m ()
sendToQueue IstatdState {..} = \case
    Counter n -> send_ queue $ Packet Increment (name n) istatdSuffixes 1
    Gauge n v -> send_ queue $ Packet Record (name n) istatdSuffixes v
    where
        name n' = istatdPrefix <> n'


runWorker :: Connection -> TBMQueue Packet -> IO ()
runWorker conn queue = go
    where
        go :: IO ()
        go = do
            let read_ = do
                    message <- atomically $ tryReadTBMQueue queue
                    return $ case message of
                        Nothing -> Nothing
                        Just Nothing -> Nothing
                        Just a -> a
                read' :: Integer -> [Packet] -> IO [Packet]
                read' !n ms = do
                    m <- read_
                    let ms' = case m of
                            Nothing -> ms
                            Just e -> e:ms
                    case n - 1 of
                        0 -> return ms'
                        !n' -> read' n' ms'
                read'' = reverse <$> read' 10 []
            messages <- read''
            case messages of
                [] ->
                    threadDelay 1000000
                messages' -> sendMany conn messages'
            >> go



