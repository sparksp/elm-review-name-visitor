# review-name-visitor

![elm-review 2.0](https://img.shields.io/badge/elm--review-2.0-%231293D8)
![elm 0.19](https://img.shields.io/badge/elm-0.19-%231293D8)
![Tests](https://github.com/sparksp/elm-review-name-visitor/workflows/Tests/badge.svg)

Visit each name in the module.

A "name" is a `Node ( ModuleName, String )` and represents any value or type reference. Here are some examples:

  - `Json.Encode.Value` -> `( [ "Json", "Encode" ], "Value" )`
  - `Html.Attributes.class` -> `( [ "Html", "Attributes" ], "class" )`
  - `Page` -> `( [], "Page" )`
  - `view` -> `( [], "view" )`

These can appear in many places throughout declarations and expressions, and picking them out each time is a lot of work.  Instead of writing 250 lines each time, you can write one `nameVisitor` and plug it straight into your module schema:

```elm
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
        |> NameVisitor.withNamVisitor nameVisitor
        |> Rule.fromModuleRuleSchema


nameVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
nameVisitor list context =
    -- Do what you want with the node found
    ( [], context )
```

This makes no attempt to resolve aliases or imports, just returns what's written in the code.
