{ name = "marionette"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "console"
  , "effect"
  , "maybe"
  , "node-readline"
  , "prelude"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
