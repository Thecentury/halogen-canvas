{ name = "halogen-project"
, dependencies =
  [
    "arrays",
    "canvas",
    "colors",
    "console",
    "css",
    "effect",
    "foldable-traversable",
    "halogen",
    "halogen-css",
    "integers",
    "lists",
    "maybe",
    "numbers",
    "prelude",
    "sized-vectors",
    "transformers",
    "typelevel",
    "unsafe-coerce",
    "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}