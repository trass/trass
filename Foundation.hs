module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Text.Shakespeare.Text (stext)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Yesod.Core.Types (Logger)
import Network.Mail.Mime

import Control.Monad (join, when)
import Data.Maybe (isJust)

import Data.Maybe (isNothing, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Encoding
import Data.Time
import Data.Int
import System.Locale

import Achievement
import ExtraPoints
import AssignmentAction
import Language
import SubmissionStatus
import UserRole
import UtilsDB

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

formatTimeFull :: UTCTime -> String
formatTimeFull = formatTime defaultTimeLocale rfc822DateFormat

getCourseIdent = lookupSession "TRASS_COURSE_IDENT"
setCourseIdent = setSession "TRASS_COURSE_IDENT"

getCourseTitle = lookupSession "TRASS_COURSE_TITLE"
setCourseTitle = setSession "TRASS_COURSE_TITLE"

getUserRole :: CourseId -> UserId -> Handler UserRole
getUserRole cid uid = do
  mentity <- runDB $ getBy $ UniqueRole uid cid
  return $ maybe RoleStudent (roleRole . entityVal) mentity

noLayout :: Widget -> Handler Html
noLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer $ pageBody pc

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        langs <- languages
        let currentLang = Text.take 2 $
              case langs of
                []    -> "en"
                (l:_) -> l
            currentLangTitle = Map.findWithDefault "English" currentLang langTitles

        courseIdent <- getCourseIdent
        mcid <-
          case courseIdent of
            Nothing -> return Nothing
            Just cname -> fmap (Just . entityKey) $ runDB $ getBy404 $ UniqueCourse cname
        courseTitle <- getCourseTitle
        mauthId <- maybeAuthId
        navBar <-
          case mauthId of
            Nothing -> widgetToPageContent $(widgetFile "anonymous/navbar")
            Just authId -> do
              userRole <-
                case mcid of
                  Nothing -> return RoleStudent
                  Just cid -> getUserRole cid authId
              (unreadMsgs, unreadPoints) <-
                case mcid of
                  Nothing -> return (0, Nothing)
                  Just cid -> do
                    m <- runDB $ countUnreadMessages cid authId userRole
                    p <- if isStudent userRole
                          then runDB $ getStudentUnreadCoursePointsSum cid authId
                          else return Nothing
                    return (m, p)
              mentity <- runDB $ getBy $ UniqueProfile authId
              let mprofile = fmap entityVal mentity
              widgetToPageContent $(widgetFile "navbar")
        footer <- widgetToPageContent $(widgetFile "footer")

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_min_css
            addStylesheet $ StaticR css_plugins_social_buttons_css
            addStylesheet $ StaticR css_trass_css
            addStylesheet $ StaticR css_font_awesome_min_css
            addScript $ StaticR js_jquery_1_11_1_min_js
            addScript $ StaticR js_bootstrap_min_js
            addScript $ StaticR assets_js_ie10_viewport_bug_workaround_js
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authenitcation.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- insertBy $ User (credsIdent creds) Nothing Nothing False
        return $ Just $
          case x of
            Left (Entity userid _)  -> userid   -- newly added user
            Right userid            -> userid   -- existing user

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authEmail {apLogin = \tm -> $(widgetFile "auth/login")}]

    authHttpManager = httpManager

instance YesodAuthPersist App

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = HomeR

    addUnverified email verkey = runDB $ insert $ User email Nothing (Just verkey) False

    sendVerifyEmail email _ verurl = do
      liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                [stext|
                    Please confirm your email address by clicking on the link below.

                    #{verurl}

                    Thank you
                |]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml
                [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you
                |]
            , partHeaders = []
            }

    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get
    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]

    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _ -> do
                update uid [UserVerified =. True]
                mp <- getBy $ UniqueProfile uid
                when (isNothing mp) $ do
                  insert_ $ Profile uid Nothing
                return $ Just uid

    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]

    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = email
                }

    getEmail = runDB . fmap (fmap userEmail) . get

    forgotPasswordHandler = do
      tp <- getRouteToParent
      lift $ authLayout $ do
        setTitleI MsgAuthForgotPasswordTitle
        $(widgetFile "auth/forgot-password")

    registerHandler = do
      tp <- getRouteToParent
      lift $ authLayout $ do
        setTitleI MsgAuthRegisterTitle
        $(widgetFile "auth/register")

    setPasswordHandler needOld = do
      tp <- getRouteToParent
      selectRep $ do
        provideRep $ lift $ authLayout $ do
          if needOld
            then setTitleI MsgAuthSetPasswordTitle
            else setTitleI MsgAuthChangePasswordTitle
          $(widgetFile "auth/set-password")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

wUserName :: UserRole -> Maybe Profile -> Maybe UserId -> Widget
wUserName role mprofile mauthId = do
  [whamlet|
    <i .fa .fa-fw :isTeacher role:.fa-graduation-cap :not (isTeacher role):.fa-user>
    $maybe name <- mprofile >>= profileName
      #{name}
    $nothing
      $maybe profile <- mprofile
        $if mauthId == Just (profileUser profile)
           _{MsgYou}
        $else
          _{unknownRole role}
      $nothing
        _{unknownRole role}
  |]
  where
    unknownRole RoleTeacher   = MsgUnknownTeacher
    unknownRole RoleAssistant = MsgUnknownAssistant
    unknownRole RoleStudent   = MsgUnknownStudent

achievementPredefinedMsg :: AchievementPredefined -> AppMessage
achievementPredefinedMsg AchievementPredefinedHelloWorld = MsgAchievementPredefinedHelloWorld
achievementPredefinedMsg AchievementPredefinedAcceptable = MsgAchievementPredefinedAcceptable
achievementPredefinedMsg _ = MsgUntitledAchievement

achievementPredefinedDescriptionMsg :: AchievementPredefined -> AppMessage
achievementPredefinedDescriptionMsg AchievementPredefinedHelloWorld = MsgAchievementPredefinedHelloWorldDescription
achievementPredefinedDescriptionMsg AchievementPredefinedAcceptable = MsgAchievementPredefinedAcceptableDescription
achievementPredefinedDescriptionMsg _ = MsgAchievementNoDescription

extraPointsPredefinedMsg :: ExtraPointsPredefined -> AppMessage
extraPointsPredefinedMsg ExtraPointsPredefinedClassActivity   = MsgExtraPointsPredefinedClassActivity
extraPointsPredefinedMsg ExtraPointsPredefinedTeacherMistake  = MsgExtraPointsPredefinedTeacherMistake
extraPointsPredefinedMsg ExtraPointsPredefinedElegantSolution = MsgExtraPointsPredefinedElegantSolution
extraPointsPredefinedMsg ExtraPointsPredefinedMissedDeadline  = MsgExtraPointsPredefinedMissedDeadline
extraPointsPredefinedMsg ExtraPointsPredefinedPlagiarism      = MsgExtraPointsPredefinedPlagiarism

