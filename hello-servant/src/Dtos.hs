{-# LANGUAGE TemplateHaskell #-}
module Dtos where
import Data.UUID
import Data.Aeson
import Data.Aeson.TH

data BudgetEnvelopeDto = BudgetEnvelopeDto { name :: String, amount :: Rational }
$(deriveJSON defaultOptions ''BudgetEnvelopeDto)

data BudgetDto = BudgetDto { currencySymbol :: String, envelopes :: [BudgetEnvelopeDto] }
$(deriveJSON defaultOptions ''BudgetDto)

data EntityDto a = EntityDto { id :: UUID, entity :: a}
$(deriveJSON defaultOptions ''EntityDto)
