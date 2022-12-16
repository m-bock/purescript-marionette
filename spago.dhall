{ name = "marionette"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "node-readline"
  , "now"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "refs"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
