{ name = "marionette"
, dependencies =
  [ "aff"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "now"
  , "ordered-collections"
  , "prelude"
  , "refs"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
