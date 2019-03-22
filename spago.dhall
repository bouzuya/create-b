{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "bouzuya-command-line-option-parser"
    , "bouzuya-datetime"
    , "bouzuya-template-string"
    , "console"
    , "effect"
    , "formatters"
    , "node-fs"
    , "node-process"
    , "now"
    , "options"
    , "psci-support"
    , "simple-json"
    , "test-unit"
    ]
, packages =
    ./packages.dhall
}
