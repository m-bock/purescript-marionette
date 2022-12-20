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
  , "node-readline"
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
