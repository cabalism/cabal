module Pot (boils) where

-- | Does water at this temperature (in degrees Celsius) boil?
boils :: Int -> Bool
boils temperature = temperature >= 100
