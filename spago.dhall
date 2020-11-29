{ name = "task-http"
, dependencies =
  [ "argonaut"
  , "foreign-object"
  , "mason-prelude"
  , "node-buffer"
  , "ordered-collections"
  , "task"
  , "undefinable"
  , "web-file"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
