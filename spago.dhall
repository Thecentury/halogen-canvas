{ name = "halogen-project"
, dependencies =
  [ "canvas"
  , "console"
  , "css"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "sized-vectors"
  , "transformers"
  , "typelevel"
  , "unsafe-coerce"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
