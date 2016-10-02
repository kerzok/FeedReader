{-# LANGUAGE OverloadedStrings #-}
module Types (Article (..), Feed (..)) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Monad       (liftM)
import           Data.Aeson          (FromJSON (..), Value (..), (.:), (.:?))

data Feed = Feed {
                feedId                  :: String,
                feedTitle               :: String,
                feedDescription         :: Maybe String,
                feedLanguage            :: Maybe String,
                feedUrl                 :: String,
                feedIconUrl             :: Maybe String,
                feedCoverUrl            :: Maybe String,
                feedLastUpdateTimestamp :: Integer,
                feedArticlesCount       :: Maybe Int
            } deriving (Eq, Show, Read)

instance FromJSON Feed where
    parseJSON (Object v) =
        Feed <$>
        (v .:  "feedId")        <*>
        (v .:  "title")         <*>
        (v .:? "description")   <*>
        (v .:? "language")      <*>
        (v .:  "website")       <*>
        (v .:? "iconUrl")       <*>
        (v .:? "coverUrl")      <*>
        (v .:  "lastUpdated")   <*>
        (v .:? "incredible")
    parseJSON  _         = fail "invalid JSON for Feed"


data Summary = Summary { content :: String }

data Cover = Cover { url :: String }

instance FromJSON Summary where
    parseJSON (Object v) = Summary <$> v .: "content"
    parseJSON _          = fail "invalid JSON for Summary"

instance FromJSON Cover where
    parseJSON (Object v) = Cover <$> v .: "url"
    parseJSON _          = fail "invalid JSON for Cover"

data Article = Article {
                    articleId            :: String,
                    feed                 :: Maybe Feed,
                    articleTitle         :: String,
                    articleAuthor        :: Maybe String,
                    articleSummary       :: Maybe String,
                    articleCoverUrl      :: Maybe String,
                    articlePublishedTime :: Integer,
                    articleUnread        :: Bool
                } deriving (Show, Read)


instance FromJSON Article where
    parseJSON (Object v) =
        Article <$>
        v .: "id"                           <*>
        pure Nothing                        <*>
        v .: "title"                        <*>
        v .:? "author"                      <*>
        liftM getContent (v .:? "summary")  <*>
        liftM getCover (v .:? "visual")     <*>
        v .: "published"                    <*>
        pure False
    parseJSON _          = fail "invalid JSON for Article"

instance Eq Article where
    article1 == article2 = articleId article1 == articleId article2

getContent :: Maybe Summary -> Maybe String
getContent Nothing = Nothing
getContent (Just summary) = Just $ content summary

getCover :: Maybe Cover -> Maybe String
getCover Nothing = Nothing
getCover (Just cover) = Just $ url cover



