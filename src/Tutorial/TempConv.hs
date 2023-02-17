{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <$>" #-}

module Tutorial.TempConv where

import Keelung

-- | Convert a temperature from Celsius to Fahrenheit.
celsiusToFahrenheit :: Comp Field
celsiusToFahrenheit = do
  degree <- input Private
  return (degree * 9 / 5 + 32)

-- | Convert a temperature from Fahrenheit to Celsius.
fahrenheitToCelsius :: Comp Field
fahrenheitToCelsius = do
  degree <- input Private
  return ((degree - 32) * 5 / 9)

-- | Convert a temperature from Celsius to Kelvin or Fahrenheit to Kelvin.
dualConvert :: Comp Field
dualConvert = do
  toCelsius <- input Private
  degree <- input Private
  return
    ( cond
        toCelsius
        (degree * 9 / 5 + 32)
        ((degree - 32) * 5 / 9)
    )
