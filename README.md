# review-name-visitor

![elm-review 2.0](https://img.shields.io/badge/elm--review-2.0-%231293D8)
![elm 0.19](https://img.shields.io/badge/elm-0.19-%231293D8)
![Tests](https://github.com/sparksp/elm-review-name-visitor/workflows/Tests/badge.svg)

Visit each name in the module.

A "name" is a `Node ( ModuleName, String )` and represents a value or type reference. Here are some examples:

  - `Json.Encode.Value` -> `( [ "Json", "Encode" ], "Value" )`
  - `Html.Attributes.class` -> `( [ "Html", "Attributes" ], "class" )`
  - `Page` -> `( [], "Page" )`
  - `view` -> `( [], "view" )`

These can appear in many places throughout declarations and expressions, and picking them out each time is a lot of work.  Instead of writing 1000 lines of code and tests each time, you can write one `nameVisitor` and plug it straight into your module schema, or separate `valueVisitor` and `typeVisitor`s.

## Documentation

### One Name Visitor

This will apply the `nameVisitor` to every value and type in the module, you will get no information about whether the name is a value or type.

```elm
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
        |> NameVisitor.withNameVisitor nameVisitor
        |> Rule.fromModuleRuleSchema


nameVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
nameVisitor node context =
    -- Do what you want with the name
    ( [], context )
```

### Separate Value and Type Visitors

This will apply the `valueVisitor` to every value and the `typeVisitor` to every type in the module.


```elm
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
        |> NameVisitor.withValueAndTypeVisitors
            { valueVisitor = valueVisitor
            , typeVisitor = typeVisitor
            }
        |> Rule.fromModuleRuleSchema


valueVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
valueVisitor node context =
    -- Do what you want with the value
    ( [], context )


typeVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
typeVisitor node context =
    -- Do what you want with the type
    ( [], context )
```

### Visit Values Only

This will apply the `valueVisitor` to every value in the module, and ignore any types.


```elm
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
        |> NameVisitor.withValueVisitor valueVisitor
        |> Rule.fromModuleRuleSchema


valueVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
valueVisitor node context =
    -- Do what you want with the value
    ( [], context )
```

### Visit Types Only

This will apply the `typeVisitor` to every type in the module, and ignore any values.


```elm
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
        |> NameVisitor.withTypeVisitor typeVisitor
        |> Rule.fromModuleRuleSchema


typeVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
typeVisitor node context =
    -- Do what you want with the type
    ( [], context )
```

## Scope

This makes no attempt to resolve module names from imports, it just returns what's written in the code.  It would be trivial to connect [elm-review-scope] with the name visitor if you want to do this.

[elm-review-scope]: http://github.com/jfmengels/elm-review-scope/

## Why this is not published as an Elm package

If this module gets published as a stand-alone package, and several rule packages start depending on it, then people may get stuck with old versions of packages until all packages catch up with any changes here. This is due to Elm not allowing duplicate dependencies with different major versions when building applications.

If the module gets exposed as part of a review package, then if another package does the same thing or the user has copied this package too for their own custom rules, then it is possible to get into module naming conflicts, which is not ideal.

I would really like you to **NOT publish this as a package**, and to **NOT expose this module** as part of your package. It is okay to publish a package using and containing this module, but without exposing it.