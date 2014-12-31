{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Applicative ((<$>))
import Control.Exception.Lifted (catch, throwIO)
import Control.Monad (when)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text, concat, splitOn, append, intercalate)
import qualified Data.Text.IO as T
import Prelude hiding (concat)
import Test.WebDriver (WDConfig, defaultConfig, defaultCaps, wdCapabilities, browser, chrome, runSession,finallyClose, WD, findElem, findElems, getText, Element, attr, openPage, click, clearInput, sendKeys, Selector(..), FailedCommandType(..), FailedCommand(..))
import Test.WebDriver.Class (WebDriver)
import Test.WebDriver.Commands.Wait (waitWhile)

myConfig :: WDConfig
myConfig = defaultConfig {
    wdCapabilities = defaultCaps {browser = chrome}
  }

main :: IO ()
main = runSession myConfig . finallyClose $ do
   setupBeforeSearch
   mapM_ searchAndPrint ["Hund", "meistern", "Nonsense"]

 
searchAndPrint :: Text -> WD ()
searchAndPrint word = do
  found <- search word
  if found 
    then do
      definition <- getFirstDefinition
      urls <- getPronUrls
      liftIO . T.putStrLn $ concat [word, "\n definition = ", definition, "\n pron URLs = "] `append` intercalate ", " urls
    else liftIO . T.putStrLn $ concat [word, "\n not found"]


getFirstDefinition :: WD Text
getFirstDefinition = findElem firstDefinition >>= getText


getPronUrls :: WD [Text]
getPronUrls = do
  playerElems <- findElems pronPlayer :: WD [Element]
  flashvars <- catMaybes <$> (mapM (`attr` "flashvars") playerElems :: WD [Maybe Text])
  return $ map extractUrl flashvars 
  where extractUrl = last . splitOn "="


setupBeforeSearch :: WD ()
setupBeforeSearch = do
  openPage "http://www.dwds.de/?qu=und"
  waitPanelsLoaded
  waitWhile 10 $ do -- close all the panels except for the first one
    closeButt <- findElem $ ByCSS ".panel_frame:nth-child(2) .panel_remove"
    click closeButt


waitPanelsLoaded :: WD ()
waitPanelsLoaded = waitWhile 5 $ findElem loadingAnimation


search :: Text -> WD Bool
search str = do
  inp <- findElem searchInput
  clearInput inp
  sendKeys str inp
  findElem searchButton >>= click
  waitPanelsLoaded
  null <$> findElems notFoundMessage


firstDefinition, loadingAnimation, notFoundMessage, pronPlayer, searchButton, searchInput :: Selector
firstDefinition  = ByClass "wb_bp"
loadingAnimation = ByClass "panel_loading"
notFoundMessage  = ByXPath "//p[contains(text(),'Kein Eintrag vorhanden')]"
pronPlayer       = ById "oneBitInsert_1"
searchButton     = ById "dwds_main_search_submit"
searchInput      = ById "query_fast_search"


{- Utility functions -}
findElemMay :: WebDriver wd => Selector -> wd (Maybe Element)
findElemMay sel = fmap Just (findElem sel) `onNoSuchElement` return Nothing

-- Convenience function to catch FailedCommand NoSuchElement exceptions and perform some action.
-- Inspired by Test.WebDriver.Commands.Wait.onTimeout
onNoSuchElement :: MonadBaseControl IO m => m a -> m a -> m a
onNoSuchElement m r = m `catch` handler
  where
    handler (FailedCommand NoSuchElement _) = r
    handler other = throwIO other
