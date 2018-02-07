{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Data.Text                   (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Conduit        (Manager, newManager)
import           Network.Wai                 (pathInfo, rawPathInfo,
                                              requestMethod)
import           Yesod

-- database Schema / Types -----------------------------------------------
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserCredentials
    userCredentialsId UserId
    userCredentialsUsername Text
    userCredentialsPassword Text
    deriving Show

User
    userName Text
|]

connStr =
    "host=localhost port=65432 user=postgres \
    \dbname=mydb password=some-password"


-- main App -----------------------------------
data App = App
   { connPool    :: ConnectionPool
   , httpManager :: Manager
   }


instance Yesod App where
    approot = ApprootStatic "http://localhost:3000"


mkYesod "App" [parseRoutes|
/ HomeR  GET
/login LoginR GET POST
|]



-- Home route ------------------------------------
getHomeR :: Handler Html
getHomeR = do
    req <- waiRequest
    defaultLayout
        [whamlet|
            <p>In any-method, method == #{show $ requestMethod req}
        |]

userCredentialsForm = renderDivs $ UserCredentials
    <$> areq textField "Username" Nothing
    <*> areq passwordField "Password" Nothing

getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost personForm
    defaultLayout
        [whamlet|
            <p>Login
            <form method=post action=@{LoginR} enctype=#{enctype}>
                ^{widget}
                 <button>Submit
        |]

postLoginR = do
    ((result, widget), enctype) <- runFormPost userCredentialsForm
    case result of
        FormSuccess userCredentials -> defaultLayout [whamlet|<p>#{show userCredentials}|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{LoginR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

main :: IO ()
main =
    runStderrLoggingT
    $ withPostgresqlPool connStr 10
    $ \pool -> liftIO $ do
        runSqlPersistMPool (runMigration migrateAll) pool
        {-
        runMigration migrateAll

        johnId <- insert $ User "John Doe"
        janeId <- insert $ User "Jane Doe"

        johnCreds <- insert $ UserCredentials johnId "john-doe" "123456"
        insert $ UserCredentials janeId "jane-d0e" "1111111111"

        creds <-
            selectList
                [UserCredentialsUserCredentialsUsername ==. "john-doe"]
                [LimitTo 1]
        liftIO $ print (creds :: [Entity UserCredentials])
        -}

        manager <- newManager tlsManagerSettings -- create a new HTTP manager
        warp 3000 $ App pool manager -- start our server





