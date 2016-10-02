{-# LANGUAGE OverloadedStrings #-}
module Database (connect,
                addFeed,
                addArticle,
                addArticles,
                getFeeds,
                deleteFeed,
                getArticles,
                getArticlesByFeedId,
                collectGarbage) where

import           Control.Concurrent
import           Control.Monad
import           Data.List
import qualified Data.Text             as T
import           Data.Text.ICU.Replace
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Network          (saveImageToDisk)
import           System.Directory
import           Types

connect :: FilePath -> IO Connection
connect filePath = do
            dbh <- connectSqlite3 filePath
            prepareDB dbh
            return dbh

addFeed :: IConnection conn => conn -> Feed -> IO ()
addFeed dbh feedToSave = handleSql errorHandler $ do
                feedIconPath <- saveImageToDisk $ feedIconUrl feedToSave
                feedCoverPath <- saveImageToDisk $ feedCoverUrl feedToSave
                commit dbh
                _ <- run dbh "INSERT INTO Feeds (FeedId, FeedTitle, FeedDescription, FeedLanguage, \
                            \FeedUrl, FeedIcon, FeedCover, FeedLastUpdate) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                        [toSql $ feedId feedToSave, toSql $ feedTitle feedToSave, toSql $ feedDescription feedToSave, toSql $ feedLanguage feedToSave,
                            toSql $ feedUrl feedToSave, toSql feedIconPath, toSql feedCoverPath, toSql $ feedLastUpdateTimestamp feedToSave]
                commit dbh
                return ()
            where errorHandler e = fail $ "Error adding feed: " ++ show e

getFeeds :: IConnection conn => conn -> IO [Feed]
getFeeds dbh = handleSql errorHandler $ do
                res <- quickQuery' dbh "SELECT *, (SELECT COUNT(*) FROM Articles WHERE Articles.FeedId = Feeds.FeedId) AS ArticlesCount FROM Feeds" []
                commit dbh
                return (map convertRowToFeed res)
            where errorHandler e = fail $ "Error getting feeds: " ++ show e

deleteFeed :: IConnection conn => conn -> String -> IO ()
deleteFeed dbh feedIdToDelete = handleSql errorHandler $ do
                quickQuery' dbh "DELETE FROM Articles WHERE FeedId = ?" [toSql feedIdToDelete]
                quickQuery' dbh "DELETE FROM Feeds WHERE FeedId = ?" [toSql feedIdToDelete]
                commit dbh
            where errorHandler e = fail $ "Error deleting feed: " ++ show e


addArticle :: IConnection conn => conn -> Article -> String -> IO ()
addArticle dbh article feedIdToSave = handleSql errorHandler $ do
                articleCoverPath <- saveImageToDisk $ articleCoverUrl article
                run dbh "INSERT INTO Articles (ArticleId, FeedId, ArticleTitle, ArticleCoverUrl, \
                            \ArticleSummary, ArticlePublished, ArticleAuthor) VALUES (?,?,?,?,?,?,?)"
                        [toSql $ articleId article, toSql feedIdToSave, toSql $ articleTitle article, toSql articleCoverPath,
                            toSql $ clearSummary $ articleSummary article, toSql $ articlePublishedTime article, toSql $ articleAuthor article]
                commit dbh
            where errorHandler e = fail $ "Error adding article: " ++ show e

addArticles :: IConnection conn => conn -> [Article] -> String -> IO ()
addArticles _ [] _ = return ()
addArticles dbh (x:xs) feedIdToSave = do
            addArticle dbh x feedIdToSave
            addArticles dbh xs feedIdToSave

getArticles :: IConnection conn => conn -> IO [Article]
getArticles dbh = handleSql errorHandler $ do
                res <- quickQuery' dbh "SELECT * FROM Articles \
                            \INNER JOIN Feeds ON Articles.FeedId = Feeds.FeedId \
                            \ORDER BY ArticlePublished DESC" []
                commit dbh
                return (map convertRowToArticle res)
            where errorHandler e = fail $ "Error getting articles: " ++ show e

getArticlesByFeedId :: IConnection conn => conn -> String -> IO [Article]
getArticlesByFeedId dbh feedIdToGet = handleSql errorHandler $ do
                res <- quickQuery' dbh "SELECT * FROM Articles \
                           \INNER JOIN Feeds ON Articles.FeedId = Feeds.FeedId \
                           \WHERE Articles.FeedId = ? \
                           \ORDER BY ArticlePublished DESC" [toSql feedIdToGet]
                commit dbh
                return (map convertRowToArticle res)
            where errorHandler e = fail $ "Error getting articles: " ++ show e

prepareDB :: IConnection conn => conn -> IO ()
prepareDB dbh = do
            tables <- getTables dbh
            unless ("Feeds" `elem` tables) $ do
                run dbh feedTableCreationQuery []
                return ()
            unless ("Articles" `elem` tables) $ do
                run dbh articleTableCreationQuery []
                return ()
            commit dbh

collectGarbage :: IConnection conn => conn -> IO ()
collectGarbage dbh = handleSql errorHandler $ do
                threadDelay defaultThreadDelay
                res <- quickQuery' dbh "SELECT DISTINCT FeedCover AS Path FROM Feeds WHERE FeedCover IS NOT NULL\
                                        \ UNION \
                                        \SELECT DISTINCT FeedIcon AS Path FROM Feeds WHERE FeedIcon IS NOT NULL\
                                        \ UNION \
                                        \SELECT DISTINCT ArticleCoverUrl AS Path FROM Articles WHERE ArticleCoverUrl IS NOT NULL" []
                commit dbh
                let storedFiles = map (\[path] -> fromSql path) res
                tempFiles <- liftM (map (\file -> "./temp/" ++ file)) $ getDirectoryContents "./temp"
                let filesToDelete = tempFiles \\ storedFiles
                removeTempImages filesToDelete
            where errorHandler e = fail $ "Error getting articles: " ++ show e

removeTempImages :: [String] -> IO ()
removeTempImages [] = return ()
removeTempImages (x:xs) = do
            checkExist <- doesFileExist x
            when checkExist $ removeFile x
            removeTempImages xs


convertRowToFeed :: [SqlValue] -> Feed
convertRowToFeed [storedId, title, description, language, url, icon, cover, lastUpdate, articles] =
            Feed {
                feedId                  = fromSql storedId,
                feedTitle               = fromSql title,
                feedDescription         = fromSql description,
                feedLanguage            = fromSql language,
                feedUrl                 = fromSql url,
                feedIconUrl             = fromSql icon,
                feedCoverUrl            = fromSql cover,
                feedLastUpdateTimestamp = fromSql lastUpdate,
                feedArticlesCount       = fromSql articles
            }
convertRowToFeed [storedId, title, description, language, url, icon, cover, lastUpdate] =
            Feed {
                feedId                  = fromSql storedId,
                feedTitle               = fromSql title,
                feedDescription         = fromSql description,
                feedLanguage            = fromSql language,
                feedUrl                 = fromSql url,
                feedIconUrl             = fromSql icon,
                feedCoverUrl            = fromSql cover,
                feedLastUpdateTimestamp = fromSql lastUpdate,
                feedArticlesCount       = Nothing
            }
convertRowToFeed x = error $ "Can't convert to Feed: " ++ show x

convertRowToArticle :: [SqlValue] -> Article
convertRowToArticle (storedArticleId:storedFeedId:title:coverUrl:summary:author:publishedTime:unread:xs) =
            Article {
                articleId            = fromSql storedArticleId,
                articleTitle         = fromSql title,
                articleAuthor        = fromSql author,
                articleSummary       = fromSql summary,
                articleCoverUrl      = fromSql coverUrl,
                articlePublishedTime = fromSql publishedTime,
                articleUnread        = fromSql unread,
                feed                 = Just (convertRowToFeed $ storedFeedId:xs)
            }
convertRowToArticle x = error $ "Can't convert to Article: " ++ show x

clearSummary :: Maybe String -> Maybe String
clearSummary (Just text) = do
            let newText = replaceAll "<br>|</br>|<hr>" "\n" $
                    replaceAll "<pre>|</pre>|<li>|</li>|<ul>|</ul>|<h[0-5]>|</h[0-5]>|<div>|</div>|<img\\s+[^>]*src=\"([^\"]*)\"[^>]*>|\n|<iframe.+?<\\/iframe>" "" $
                    replaceAll "<em>" "<i>" $
                    replaceAll "</em>" "</i>" $
                    replaceAll "<code>" "<b><i>" $
                    replaceAll "</code>" "</i></b>" $
                    T.pack text
            return $ T.unpack $ T.strip newText
clearSummary Nothing = Nothing

defaultThreadDelay :: Int
defaultThreadDelay = 50 * 10000

feedTableCreationQuery :: String
feedTableCreationQuery = "CREATE TABLE IF NOT EXISTS Feeds (" ++
                    "FeedId TEXT NOT NULL PRIMARY KEY," ++
                    "FeedTitle TEXT NOT NULL," ++
                    "FeedDescription TEXT DEFAULT NULL," ++
                    "FeedLanguage TEXT DEFAULT NULL," ++
                    "FeedUrl TEXT NOT NULL," ++
                    "FeedIcon TEXT DEFAULT NULL," ++
                    "FeedCover TEXT DEFAULT NULL," ++
                    "FeedLastUpdate INTEGER NOT NULL)"

articleTableCreationQuery :: String
articleTableCreationQuery = "CREATE TABLE IF NOT EXISTS Articles (" ++
                    "ArticleId TEXT NOT NULL PRIMARY KEY," ++
                    "FeedId TEXT NOT NULL," ++
                    "ArticleTitle TEXT NOT NULL," ++
                    "ArticleCoverUrl TEXT DEFAULT NULL," ++
                    "ArticleSummary TEXT DEFAULT NULL," ++
                    "ArticleAuthor TEXT DEFAULT NULL," ++
                    "ArticlePublished INTEGER NOT NULL," ++
                    "ArticleUnread BIT DEFAULT 0)"
