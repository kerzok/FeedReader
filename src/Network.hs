{-# LANGUAGE OverloadedStrings #-}
module Network(searchFeeds, loadArticles, saveImageToDisk) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List
import           Data.Maybe
import           Data.UUID
import           Data.UUID.V1
import           Network.Wreq
import           Types
import           System.Directory
import           Control.Monad              (unless)
import           Control.Applicative        ((<$>))
import           Data.Aeson                 (FromJSON (..), Value (..), (.:))

newtype FeedList = FeedList { feeds :: [Feed]}
newtype ArticleList = ArticleList { articles :: [Article]}
type FeedListResp = Response FeedList
type ArticleListResp = Response ArticleList

instance FromJSON FeedList where
    parseJSON (Object v) = FeedList <$> v .: "results"
    parseJSON _          = fail "invalid JSON for FeedList"

instance FromJSON ArticleList where
    parseJSON (Object v) = ArticleList <$> v .: "items"
    parseJSON _          = fail "invalid JSON for ArticlesList"


createSearchFeedRequestUrl :: String -> String
createSearchFeedRequestUrl query = "http://cloud.feedly.com/v3/search/feeds?query=" ++ query ++ "&count=20"

createGetArticlesRequestUrl :: String -> String
createGetArticlesRequestUrl feedIdToUpdate = "http://cloud.feedly.com/v3/streams/contents?streamId=" ++ feedIdToUpdate ++ "&count=50"

searchFeeds :: String -> IO [Feed]
searchFeeds query = let url = createSearchFeedRequestUrl query in do
            r <- asJSON =<< get url :: IO FeedListResp
            return $ feeds (r ^. responseBody)

loadArticles :: String -> IO [Article]
loadArticles feedIdToUpdate = let url = createGetArticlesRequestUrl feedIdToUpdate in do
            r <- asJSON =<< get url :: IO ArticleListResp
            return $ articles (r ^. responseBody)

saveImageToDisk :: Maybe String -> IO (Maybe String)
saveImageToDisk Nothing = return Nothing
saveImageToDisk (Just url) = if "http" `isPrefixOf` url then do
                                response <- get url
                                genName <- generateName
                                checkDirectory <- doesDirectoryExist "./temp"
                                unless checkDirectory $ createDirectory "./temp"
                                B.writeFile ("./temp/" ++ genName) (response ^. responseBody)
                                return $ Just ("./temp/" ++ genName)
                              else return Nothing

generateName :: IO String
generateName = do
        g <- nextUUID
        return $ take 8 $ toString $ fromJust g
