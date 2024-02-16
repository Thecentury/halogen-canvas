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
  , "lists"
  , "maybe"
  , "prelude"
  , "sized-vectors"
  , "tailrec"
  , "transformers"
  , "typelevel"
  , "unsafe-coerce"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}