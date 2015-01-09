{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text, concat, splitOn, append, pack)
import qualified Data.Text.IO as T
import Prelude hiding (concat)
import Test.WebDriver (runSession,finallyClose, WD, findElem, findElems, getText, Element, attr, openPage, click, Selector(..))
import Test.WebDriver.Commands.Wait (waitWhile)

import WdUtil (clickElem, setInput, myWdConfig)

main :: IO ()
main = runSession myWdConfig . finallyClose $ do
   setupBeforeSearch
   wordData <- mapM extractWordData ["Hund", "meistern", "Nonsense", "Gehweg"]
   liftIO . mapM_ (T.putStrLn . formatWordData) $ wordData

formatWordData :: WordData -> Text
formatWordData Word {wWord = w, wPronUrls = prons, wDefinitions = defs} = w `append`
  if all null [prons, defs]
    then " NOT FOUND"
    else fmt "pron" prons `append` fmt "def" defs
    where fmt label items = concat $ zipWith (\ item index -> concat ["\n  ", label, index, " = ", item]) items textIdxs
          textIdxs = map (pack . show) [1..]
 
extractWordData :: Text -> WD WordData
extractWordData word = do
  search word
  defs <- getDefinitions
  pronUrls <- getPronUrls
  return Word {wWord = word, wPronUrls = pronUrls, wDefinitions = defs}

data WordData = Word 
    { wWord :: Text
    , wPronUrls :: [Text]
    , wDefinitions :: [Text]
    }

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


search :: Text -> WD ()
search str = do
  setInput searchInput str 
  clickElem searchButton
  waitPanelsLoaded

definition, loadingAnimation, {-notFoundMessage,-} pronPlayer, searchButton, searchInput :: Selector
definition       = ByCSS "div[id^=sense]>.wb_bp"
loadingAnimation = ByClass "panel_loading"
--notFoundMessage  = ByXPath "//p[contains(text(),'Kein Eintrag vorhanden')]"
pronPlayer       = ById "oneBitInsert_1"
searchButton     = ById "dwds_main_search_submit"
searchInput      = ById "query_fast_search"
