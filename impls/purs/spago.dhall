{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mal"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "now"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "refs"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
