{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "canvas"
  , "colors"
  , "console"
  , "css"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "sized-vectors"
  , "st"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "unsafe-coerce"
  , "web-events"
  , "web-html"
  , "web-touchevents"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
