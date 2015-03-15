{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import Data.Text (Text)
import Yesod
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

-- cabal run
-- http://localhost:3000/quadrest/1

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Quad
  subject String
  predicate String
  object String
  deriving Show
|]

connStr = "host=localhost dbname=yesod user=markus password=mark port=5432"

data PgRest = PgRest ConnectionPool

mkYesod "PgRest" [parseRoutes|
/ HomeR GET
/quad/#QuadId QuadR GET
/quadrest/#QuadId QuadRestR GET
|]

instance Yesod PgRest

instance YesodPersist PgRest where
  type YesodPersistBackend PgRest = SqlBackend

  runDB action = do
    PgRest pool <- getYesod
    runSqlPool action pool

getHomeR :: Handler Html
getHomeR = do
  quads <- runDB $ selectList [] []
  defaultLayout
    [whamlet|
        <ul>
            $forall Entity quadid quad <- quads
                <li>
                    <a href=@{QuadR quadid}>#{quadSubject quad}
    |]

getQuadR :: QuadId -> Handler String
getQuadR quadId = do
  quad <- runDB $ get404 quadId
  return $ show quad

getQuadRestR :: QuadId -> Handler Value
getQuadRestR quadId = do
  maybeQuad <- runDB $ get quadId
  return $ object
    [ "subject" .= case maybeQuad of
                       Just quad -> quadSubject quad
                       _   -> ""
      , "predicate" .= case maybeQuad of
                       Just quad -> quadPredicate quad
                       _   -> ""
      , "object" .= case maybeQuad of
                       Just quad -> quadPredicate quad
                       _   -> ""]

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    insert $ Quad "John" "Predicate" "Object"
  warp 3000 $ PgRest pool  
