{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Applicative ((<$>))
import Control.Exception.Lifted (catch, throwIO)
import Control.Monad (forM, when)
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
   wordData <- mapM extractWordData ["Hund", "meistern", "Nonsense", "Gehweg"]
   liftIO . mapM_ (T.putStrLn . formatWordData) $ wordData

formatWordData :: WordData -> Text
formatWordData (("word", w) : wdata) = w `append` 
  case wdata of
    []        -> " NOT FOUND"
    otherwise -> concat $ map (\(field, value) -> concat ["\n  ",field, " = ", value]) wdata
 
extractWordData :: Text -> WD WordData
extractWordData word = do
  found <- search word
  extract <- if found then do
      definition <- zip (repeat "definition") <$> getDefinitions
      urls <- zip (repeat "url") <$> getPronUrls
      return $ definition ++ urls
    else return []
  return $ ("word", word) : extract

type WordData = [(Text, Text)]

getDefinitions :: WD [Text]
getDefinitions = findElems definition >>= mapM getText


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


definition, loadingAnimation, notFoundMessage, pronPlayer, searchButton, searchInput :: Selector
definition       = ByCSS "div[id^=sense]>.wb_bp"
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
