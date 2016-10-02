module AddNewFeedActivity (AddNewFeedActivity.init) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Data.Maybe
import qualified Data.Text           as T
import           Database
import           Graphics.UI.Gtk     hiding (disconnect)
import           Network
import           Types

init :: String -> IO ()
init gladePath = do
        initGUI
        builder <- builderNew
        builderAddFromFile builder gladePath
        mainWindow <- builderGetObject builder castToWindow "addNewFeedActivity"
        searchEntry <- builderGetObject builder castToEntry "search"
        on searchEntry keyPressEvent $ tryEvent $ do "Return" <- liftM T.unpack eventKeyName
                                                     liftIO $  feedInitSearchEvent searchEntry builder
        windowSetDefaultSize mainWindow 640 480
        widgetShowAll mainWindow
        forkOS mainGUI
        return ()

feedInitSearchEvent :: Entry -> Builder -> IO ()
feedInitSearchEvent searchEntry builder = do
        searchText <- liftM T.unpack $ entryGetText searchEntry
        when (length searchText > 3) $ do
                viewPort <- builderGetObject builder castToContainer "viewport"
                spinnerInit viewPort
                forkIO $ do
                    feeds <- searchFeeds searchText
                    if not $ null feeds then
                        feedInitList builder feeds viewPort
                    else do
                        noResultsLabel <- labelNew $ Just "Ничего не найдено :("
                        postGUIAsync $ do
                            containerForeach viewPort $ containerRemove viewPort
                            containerAdd viewPort noResultsLabel
                            widgetShowAll viewPort
                return ()

spinnerInit :: Container -> IO ()
spinnerInit container = do
        containerForeach container $ containerRemove container
        spinner <- spinnerNew
        spinnerStart spinner
        containerAdd container spinner
        widgetShowAll container

feedInitList :: Builder -> [Feed] -> Container -> IO ()
feedInitList builder feeds parent = do
        box <- vBoxNew False 10
        boxSetSpacing box 50
        mapM_ (feedSetToBox builder box) feeds
        postGUIAsync $ do
            containerForeach parent $ containerRemove parent
            containerAdd parent box
            widgetShowAll parent
        return ()

feedSetToBox :: Builder -> VBox -> Feed -> IO EventBox
feedSetToBox builder parent feedToSet = do
        box <- vBoxNew False 10
        cover <- saveImageToDisk $ feedCoverUrl feedToSet
        when (isJust cover) $ do
            loadPixResult <- try (pixbufNewFromFileAtSize (fromJust cover) 320 240) :: IO (Either SomeException Pixbuf)
            case loadPixResult of
                Left _ -> return ()
                Right pix -> do
                    image <- imageNewFromPixbuf pix
                    containerAdd box image
        filledBox <- feedBoxSetData box feedToSet
        eventBox <- eventBoxNew
        on eventBox buttonPressEvent $ tryEvent $ liftIO $ do
            connection <- connect "feeds.db"
            addFeed connection feedToSet
            mainWindow <- builderGetObject builder castToWindow "addNewFeedActivity"
            widgetDestroy mainWindow
        containerAdd eventBox filledBox
        containerAdd parent eventBox
        return eventBox

feedBoxSetData :: VBox -> Feed -> IO VBox
feedBoxSetData box feedToSet = do
        title <- labelNew $ Just $ "<b>" ++ feedTitle feedToSet ++ "</b>"
        labelSetJustify title JustifyLeft
        set title [labelUseMarkup := True]
        containerAdd box title

        description <- labelNew $ feedDescription feedToSet
        labelSetLineWrap description True
        labelSetLineWrapMode description WrapPartialWords
        labelSetJustify description JustifyLeft
        labelSetWidthChars description 10
        set description [labelUseMarkup := True,
                     labelLineWrap := True,
                     labelJustify := JustifyLeft,
                     labelWidthChars := 10]
        labelSetLineWrapMode description WrapPartialWords
        containerAdd box description

        set box [widgetMarginLeft := 15,
                 widgetMarginRight := 15]
        containerSetBorderWidth box 5
        return box


