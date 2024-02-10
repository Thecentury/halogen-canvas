{ name = "halogen-project"
, dependencies =
  [ "console"
  , "css"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "integers"
  , "maybe"
  , "prelude"
  , "sized-vectors"
  , "transformers"
  , "typelevel"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
