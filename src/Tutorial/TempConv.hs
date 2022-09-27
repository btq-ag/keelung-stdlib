{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use <$>" #-}

module Tutorial.TempConv where

import Keelung

-- | Convert a temperature from Celsius to Fahrenheit.
celsiusToFahrenheit :: Comp Number
celsiusToFahrenheit = do
  degree <- input
  return (degree * 9 / 5 + 32)

-- | Convert a temperature from Fahrenheit to Celsius.
fahrenheitToCelsius :: Comp Number
fahrenheitToCelsius = do
  degree <- input
  return ((degree - 32) * 5 / 9)

-- | Convert a temperature from Celsius to Kelvin or Fahrenheit to Kelvin.
dualConvert :: Comp Number
dualConvert = do
  toCelsius <- input
  degree <- input
  return
    ( cond
        toCelsius
        (degree * 9 / 5 + 32)
        ((degree - 32) * 5 / 9)
    )
