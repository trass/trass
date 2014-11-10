module Handler.Language where

import Import
import Language

import qualified Data.Map as Map

getLanguageR :: Handler Html
getLanguageR = do
  let langs = Map.toList langTitles
  defaultLayout $ do
    $(widgetFile "language")

postLanguageR :: Handler Html
postLanguageR = do
  mlang <- lookupPostParam "lang"
  case mlang of
    Just lang -> setLanguage lang
    Nothing -> return ()
  redirect HomeR
