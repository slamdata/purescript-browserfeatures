module Data.BrowserFeatures.InputType
  ( InputType(..)
  , renderInputType
  , allInputTypes
  ) where

import Prelude

data InputType
  = Color
  | Date
  | DateTimeLocal
  | Time
  | Month
  | Week
  | Email
  | Url
  | Number
  | Search
  | Range
  | Text

allInputTypes :: Array InputType
allInputTypes =
  [ Color
  , Date
  , DateTimeLocal
  , Time
  , Month
  , Week
  , Email
  , Url
  , Number
  , Search
  , Range
  , Text
  ]

derive instance eqInputType :: Eq InputType
derive instance ordInputType :: Ord InputType

instance showInputType :: Show InputType where
  show Color = "Color"
  show Date = "Date"
  show DateTimeLocal = "DateTimeLocal"
  show Time = "Time"
  show Month = "Month"
  show Week = "Week"
  show Email = "Email"
  show Url = "Url"
  show Number = "Number"
  show Search = "Search"
  show Range = "Range"
  show Text = "Text"

-- | Render an `InputType` into the corresponding value of the `type` attribute
-- | on an `input` element.
renderInputType :: InputType -> String
renderInputType Color = "color"
renderInputType Date = "date"
renderInputType DateTimeLocal = "datetime-local"
renderInputType Time = "time"
renderInputType Month = "month"
renderInputType Week = "week"
renderInputType Email = "email"
renderInputType Url = "url"
renderInputType Number = "number"
renderInputType Search = "search"
renderInputType Range = "range"
renderInputType Text = "text"
