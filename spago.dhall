{ name = "task-http"
, dependencies =
  [ "argonaut"
  , "foreign-object"
  , "mason-prelude"
  , "node-buffer"
  , "nullable"
  , "ordered-collections"
  , "task"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
