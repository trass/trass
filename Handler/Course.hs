module Handler.Course where

import Import

getCourseR :: Text -> Handler Html
getCourseR ident = do
  Entity _ course  <- runDB $ getBy404 $ UniqueCourse ident
  section <- runDB $ get404 $ courseRootSection course
  defaultLayout $ do
    $(widgetFile "course-section")
