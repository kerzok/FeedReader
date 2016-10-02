module MainActivity (MainActivity.init) where


import           AddNewFeedActivity
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans       (liftIO)
import           Data.List
import           Data.Maybe
import           Database
import           Graphics.UI.Gtk           hiding (disconnect)
import           Graphics.UI.Gtk.ModelView as Model
import           Network
import           System.Exit
import           Types

type ListStoreItem = IO (String, String, Maybe Pixbuf)

showListStoreItem :: ListStoreItem -> IO String
showListStoreItem item = do
            (first, second, _) <- item
            return $ first


init :: FilePath -> IO ()
init gladePath = do
            initGUI
            builder <- builderNew
            builderAddFromFile builder gladePath
            mainWindow <- builderGetObject builder castToWindow "mainActivity"
            addFeedButton <- builderGetObject builder castToButton "addNewFeedButton"
            on addFeedButton buttonActivated $ AddNewFeedActivity.init "src/activity_add_new_feed.glade"
            windowSetDefaultSize mainWindow 900 600
            widgetShowAll mainWindow
            exit <- newEmptyMVar
            on mainWindow focusInEvent $ tryEvent $ liftIO $ onFocusActivatedHandler builder
            on mainWindow deleteEvent $ liftIO $ putMVar exit ExitSuccess >> return False
            forkOS mainGUI
            signal <- takeMVar exit
            postGUIAsync mainQuit
            exitWith signal

onFocusActivatedHandler :: Builder -> IO ()
onFocusActivatedHandler builder = do
                restoreFeedModel builder "focus"
                connection <- connect "feeds.db"
                forkOS $ collectGarbage connection
                return ()


restoreFeedModel :: Builder -> String -> IO ()
restoreFeedModel builder from = do
            treeView <- builderGetObject builder castToTreeView "feedList"
            refreshButton <- builderGetObject builder castToButton "refreshButton"
            test <- builderGetObject builder castToLabel "test"

            feedModel <- feedInitTreeView builder
            arr <- listStoreToList feedModel
            showableArray <- mapM showListStoreItem arr
            labelSetLabel test $ show showableArray
            on refreshButton buttonActivated $ liftIO $ articlesRefresh refreshButton builder feedModel
            pop <- createContextMenu builder feedModel
            on treeView buttonPressEvent $ do
                  button <- eventButton
                  coord <- eventCoordinates
                  case button of
                    RightButton -> liftIO $ (do
                                    isSelected <- setSelectionByCoord builder coord
                                    when isSelected $ menuPopup (castToMenu pop) Nothing) >> return True
                    _           -> liftIO $ (do
                                    isSelected <- setSelectionByCoord builder coord
                                    when isSelected $ do
                                        maybeFeedId <- getFeedIdFromTreeView builder feedModel
                                        --labelSetLabel test $ show maybeFeedId
                                        when (isJust maybeFeedId) $ do
                                            let itemId = fromJust maybeFeedId
                                            viewPort <- builderGetObject builder castToContainer "viewport"
                                            containerForeach viewPort $ containerRemove viewPort
                                            forkIO $ articleInitList viewPort itemId
                                            return ()) >> return True

            return ()


createContextMenu :: Builder -> ListStore ListStoreItem -> IO Widget
createContextMenu builder feedModel = do
            treeView <- builderGetObject builder castToTreeView "feedList"
            deleteAction <- actionNew "Delete" "Удалить" Nothing Nothing
            agr <- actionGroupNew "AGR1"
            actionGroupAddAction agr deleteAction
            uiman <- uiManagerNew
            uiManagerAddUiFromString uiman uiDecl
            uiManagerInsertActionGroup uiman agr 0
            maybePopup <- uiManagerGetWidget uiman "/ui/popup"
            let pop = case maybePopup of
                        (Just x) -> x
                        Nothing -> error "Cannot get popup from string"
            on deleteAction actionActivated $ processAction builder feedModel
            return pop

articlesRefresh :: Button -> Builder -> ListStore ListStoreItem -> IO ()
articlesRefresh button builder feedModel = do
             treeView <- builderGetObject builder castToTreeView "feedList"
             maybeFeedId <- getFeedIdFromTreeView builder feedModel
             when (isJust maybeFeedId) $ do
                let itemId = fromJust maybeFeedId
                articlesLoadNew button itemId builder

articlesLoadNew :: Button -> String -> Builder -> IO ()
articlesLoadNew button feedIdToUpdate builder = do
            spinner <- spinnerInit builder
            widgetSetSensitive button False
            forkIO $ do
                connection <- connect "feeds.db"
                articles <- loadArticles feedIdToUpdate
                storedArticles <- getArticlesByFeedId connection feedIdToUpdate
                let articlesToInsert = articles \\ storedArticles
                addArticles connection articlesToInsert feedIdToUpdate
                postGUIAsync $ do
                    spinnerStop spinner
                    widgetSetSensitive button True
                    viewPort <- builderGetObject builder castToContainer "viewport"
                    containerForeach viewPort $ containerRemove viewPort
                    forkIO $ articleInitList viewPort feedIdToUpdate
                    return ()
            return ()

spinnerInit :: Builder -> IO Spinner
spinnerInit builder = do
            container <- builderGetObject builder castToContainer "spinnerContainer"
            containerForeach container $ containerRemove container
            spinner <- spinnerNew
            spinnerStart spinner
            containerAdd container spinner
            widgetShowAll container
            return spinner

feedInitTreeView :: Builder -> IO (ListStore ListStoreItem)
feedInitTreeView builder = do
            treeView <- builderGetObject builder castToTreeView "feedList"
            columns <- treeViewGetColumns treeView
            mapM_ (treeViewRemoveColumn treeView) columns
            connection <- connect "feeds.db"
            feeds <- getFeeds connection
            list <- listStoreNew $ map feedConvertToListStoreItem feeds
            column <- Model.treeViewColumnNew
            cellIcon <- Model.cellRendererPixbufNew
            cellLayoutPackStart column cellIcon True
            treeViewSetHeadersVisible treeView True
            cellLayoutSetAttributeFunc column cellIcon list $ \it -> do
                                item <- treeModelGetRow list it
                                (_, _, icon) <- item
                                when (isJust icon) $
                                    set cellIcon [cellPixbuf := fromJust icon]

            cellTitle <- Model.cellRendererTextNew
            cellLayoutPackStart column cellTitle True
            cellLayoutSetAttributeFunc column cellTitle list $ \it -> do
                                item <- treeModelGetRow list it
                                (_, title, _) <- item
                                set cellTitle [cellText := title]

            treeViewAppendColumn treeView column
            treeViewSetModel treeView list
            return list

setSelectionByCoord :: Builder -> (Double, Double) -> IO Bool
setSelectionByCoord builder (x, y) = do
            treeView <- builderGetObject builder castToTreeView "feedList"
            maybePath <- treeViewGetPathAtPos treeView (round x, round y)
            if isJust maybePath then do
                let (realPath, _, _) = fromJust maybePath
                treeSelection <- treeViewGetSelection treeView
                treeSelectionSelectPath treeSelection realPath
                return True
            else
                return False

getFeedIdFromTreeView :: Builder -> ListStore ListStoreItem -> IO (Maybe String)
getFeedIdFromTreeView builder feedModel = do
            test <- builderGetObject builder castToLabel "test"
            treeView <- builderGetObject builder castToTreeView "feedList"
            model <- treeViewGetModel treeView
            selection <- treeViewGetSelection treeView
            iter <- treeSelectionGetSelected selection
            if isJust iter then do
                index <- liftM getFirstItem $ treeModelGetPath (fromJust model) $ fromJust iter
                postGUIAsync $ do
                    labelSetLabel test $ show index
                    return ()
                if index >= 0 then do
                    item <- listStoreSafeGetValue feedModel index
                    if isJust item then do
                        (itemId, _, _) <- fromJust item
                        return (Just itemId)
                    else return Nothing
                else
                    return Nothing
            else
                return Nothing

processAction :: Builder -> ListStore ListStoreItem -> IO ()
processAction builder list = do
            maybeFeedId <- getFeedIdFromTreeView builder list
            when (isJust maybeFeedId) $ do
                let itemId = fromJust maybeFeedId
                feedActionDelete builder itemId
                return ()

feedActionDelete :: Builder -> String -> IO ThreadId
feedActionDelete builder feedIdToDelete = forkIO $ do
            connection <- connect "feeds.db"
            deleteFeed connection feedIdToDelete
            postGUIAsync $ restoreFeedModel builder "delete"
            return ()

feedConvertToListStoreItem :: Feed -> ListStoreItem
feedConvertToListStoreItem feedToConvert
            | isJust $ feedIconUrl feedToConvert = do
                    loadPixResult <- try (pixbufNewFromFile $ fromJust $ feedIconUrl feedToConvert) :: IO (Either SomeException Pixbuf)
                    case loadPixResult of
                        Left _ -> return (feedId feedToConvert, feedTitle feedToConvert, Nothing)
                        Right pix -> return (feedId feedToConvert, feedTitle feedToConvert, Just pix)
            | otherwise = return (feedId feedToConvert, feedTitle feedToConvert, Nothing)

articleInitList :: Container -> String -> IO ()
articleInitList parent feedIdToInit = do
            connection <- connect "feeds.db"
            articles <- getArticlesByFeedId connection feedIdToInit
            box <- articleFillList articles
            postGUISync $ do
                containerForeach parent $ containerRemove parent
                containerAdd parent box
                widgetShowAll parent
            return ()

articleFillList :: [Article] -> IO VBox
articleFillList articles = do
            box <- vBoxNew False 10
            boxSetSpacing box 50
            mapM_ (articleSetToBox box) articles
            return box

articleSetToBox :: VBox -> Article -> IO VBox
articleSetToBox parent article
            | isJust $ articleCoverUrl article = do
                    box <- vBoxNew False 0
                    loadPixResult <- try (pixbufNewFromFileAtSize (fromJust $ articleCoverUrl article) 320 240) :: IO (Either SomeException Pixbuf)
                    case loadPixResult of
                        Left _ -> articleBoxSetData parent box article
                        Right pix -> do
                            image <- imageNewFromPixbuf pix
                            containerAdd box image
                            articleBoxSetData parent box article
            | otherwise = do
                    box <- vBoxNew False 0
                    articleBoxSetData parent box article

articleBoxSetData :: VBox -> VBox -> Article -> IO VBox
articleBoxSetData parent box article = do
            title <- labelNew $ Just $ "<b>" ++ articleTitle article ++ "</b>"
            labelSetJustify title JustifyLeft
            set title [labelUseMarkup := True]
            containerAdd box title

            summary <- labelNew $ articleSummary article
            labelSetLineWrap summary True
            labelSetLineWrapMode summary WrapPartialWords
            labelSetJustify summary JustifyLeft
            labelSetSelectable summary True
            labelSetWidthChars summary 10
            set summary [labelUseMarkup := True,
                         labelLineWrap := True,
                         labelJustify := JustifyLeft,
                         labelWidthChars := 10]
            labelSetLineWrapMode summary WrapPartialWords
            containerAdd box summary

            set box [widgetMarginLeft := 15,
                     widgetMarginRight := 15]
            containerSetBorderWidth box 5
            containerAdd parent box
            return box

getFirstItem :: TreePath -> Int
getFirstItem (x:_) = x
getFirstItem _ = -1

uiDecl :: String
uiDecl = "<ui> \
\          <popup>\
\            <menuitem action=\"Delete\" />\
\          </popup>\
\        </ui>"








