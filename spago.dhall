{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "canvas"
  , "colors"
  , "console"
  , "css"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "halogen-subscriptions"
  , "integers"
  , "maybe"
  , "prelude"
  , "sized-vectors"
  , "st"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "unsafe-coerce"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}