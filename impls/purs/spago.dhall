{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "freet"
  , "identity"
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
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
