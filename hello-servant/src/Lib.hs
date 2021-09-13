{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.TypeLits
import Data.UUID
import Dtos
import Data.UUID.V4
import Control.Monad.IO.Class (liftIO)

type PostBudgetEndpoint = "budget" :> ReqBody '[JSON] BudgetDto :> Post '[JSON] (EntityDto BudgetDto) 
type GetBudgetEndpoint = "budget/:id" :> Capture "id" UUID :> Get '[JSON] (EntityDto BudgetDto) 

type API = PostBudgetEndpoint :<|> GetBudgetEndpoint

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = postBudgetHandler :<|> getBudgetHandler


postBudgetHandler :: BudgetDto -> Handler (EntityDto BudgetDto )
postBudgetHandler dto = do
  id <- liftIO nextRandom
  return $ EntityDto id dto

sampleBudgetDto :: BudgetDto
sampleBudgetDto = BudgetDto { currencySymbol = "PLN", envelopes = []}

getBudgetHandler :: UUID -> Handler (EntityDto BudgetDto)
getBudgetHandler id = do
  return $ EntityDto id sampleBudgetDto
