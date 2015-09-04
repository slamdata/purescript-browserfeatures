## Module Data.BrowserFeatures.InputType

#### `InputType`

``` purescript
data InputType
  = Color
  | Date
  | DateTime
  | DateTimeLocal
  | Time
  | Month
  | Week
  | Email
  | Url
  | Number
  | Search
  | Range
```

##### Instances
``` purescript
instance showInputType :: Show InputType
instance eqInputType :: Eq InputType
instance ordInputType :: Ord InputType
```

#### `allInputTypes`

``` purescript
allInputTypes :: Array InputType
```

#### `renderInputType`

``` purescript
renderInputType :: InputType -> String
```

Render an `InputType` into the corresponding value of the `type` attribute
on an `input` element.


