module NameVisitor exposing
    ( withNameVisitor
    , declarationListVisitor, expressionVisitor
    )

{-| Visit each call in the module.

A "call" is a `Node ( ModuleName, String )` and represents any value or type reference. Here are some examples:

  - `Json.Encode.Value` -> `( [ "Json", "Encode" ], "Value" )`
  - `Html.Attributes.class` -> `( [ "Html", "Attributes" ], "class" )`
  - `Page` -> `( [], "Page" )`
  - `view` -> `( [], "view" )`

These can appear in many places throughout declarations and expressions, and picking them out each time is a lot of work.
Instead of writing 250 lines each time, you can write one `nameVisitor` and plug it straight into your module schema:

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
            |> NameVisitor.withNameVisitor nameVisitor
            |> Rule.fromModuleRuleSchema

    callListVisitor : Node ( ModuleName, String ) -> context -> ( List (Error {}), context )
    callListVisitor node context =
        -- Do what you want with the node found
        ( [], context )

@docs withNameVisitor

@docs declarationListVisitor, expressionVisitor

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Rule as Rule exposing (Error)


{-| Adds a `declarationListVisitor` and an `expressionVisitor`.
-}
withNameVisitor :
    (Node ( ModuleName, String ) -> context -> ( List (Error {}), context ))
    -> Rule.ModuleRuleSchema { schemaState | canCollectProjectData : () } context
    -> Rule.ModuleRuleSchema { schemaState | canCollectProjectData : (), hasAtLeastOneVisitor : () } context
withNameVisitor visitor rule =
    rule
        |> Rule.withDeclarationListVisitor (declarationListVisitor visitor)
        |> Rule.withExpressionVisitor (expressionVisitor visitor)


{-| A declaration list visitor that will run your call visitor.

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
            |> Rule.withDeclarationListVisitor (declarationListVisitor nameVisitor)
            |> Rule.fromModuleRuleSchema

-}
declarationListVisitor :
    (Node ( ModuleName, String ) -> context -> ( List (Error {}), context ))
    -> (List (Node Declaration) -> context -> ( List (Error {}), context ))
declarationListVisitor visitor list context =
    visitDeclarationList list
        |> folder visitor context


{-| An expression visitor that will run your call visitor.

    rule : Rule
    rule =
        Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
            |> Rule.withExpressionVisitor (expressionVisitor nameVisitor)
            |> Rule.fromModuleRuleSchema

-}
expressionVisitor :
    (Node ( ModuleName, String ) -> context -> ( List (Error {}), context ))
    -> (Node Expression -> Rule.Direction -> context -> ( List (Error {}), context ))
expressionVisitor visitor node direction context =
    case direction of
        Rule.OnEnter ->
            visitExpression node
                |> folder visitor context

        Rule.OnExit ->
            [] |> folder visitor context



--- FOLDER


folder :
    (Node ( ModuleName, String ) -> context -> ( List (Error {}), context ))
    -> (context -> List (Node ( ModuleName, String )) -> ( List (Error {}), context ))
folder visitor context list =
    case list of
        [] ->
            ( [], context )

        head :: rest ->
            let
                ( headErrors, headContext ) =
                    visitor head context

                ( restErrors, restContext ) =
                    folder visitor headContext rest
            in
            ( headErrors ++ restErrors, restContext )



--- PRIVATE


visitDeclarationList : List (Node Declaration) -> List (Node ( ModuleName, String ))
visitDeclarationList nodes =
    List.concatMap visitDeclaration nodes


visitDeclaration : Node Declaration -> List (Node ( ModuleName, String ))
visitDeclaration node =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            visitMaybeSignature signature
                ++ visitFunctionImplementation declaration

        Declaration.AliasDeclaration { typeAnnotation } ->
            visitTypeAnnotation typeAnnotation

        Declaration.CustomTypeDeclaration { constructors } ->
            visitValueConstructorList constructors

        Declaration.PortDeclaration { typeAnnotation } ->
            visitTypeAnnotation typeAnnotation

        _ ->
            []


visitMaybeSignature : Maybe (Node Signature) -> List (Node ( ModuleName, String ))
visitMaybeSignature maybeNode =
    case maybeNode of
        Just node ->
            visitSignature node

        Nothing ->
            []


visitSignature : Node Signature -> List (Node ( ModuleName, String ))
visitSignature node =
    visitTypeAnnotation (node |> Node.value |> .typeAnnotation)


visitFunctionImplementation : Node Expression.FunctionImplementation -> List (Node ( ModuleName, String ))
visitFunctionImplementation node =
    visitPatternList (node |> Node.value |> .arguments)


visitValueConstructorList : List (Node Type.ValueConstructor) -> List (Node ( ModuleName, String ))
visitValueConstructorList list =
    List.concatMap visitValueConstructor list


visitValueConstructor : Node Type.ValueConstructor -> List (Node ( ModuleName, String ))
visitValueConstructor node =
    visitTypeAnnotationList (node |> Node.value |> .arguments)


visitTypeAnnotationList : List (Node TypeAnnotation) -> List (Node ( ModuleName, String ))
visitTypeAnnotationList list =
    List.concatMap visitTypeAnnotation list


visitTypeAnnotation : Node TypeAnnotation -> List (Node ( ModuleName, String ))
visitTypeAnnotation node =
    case Node.value node of
        TypeAnnotation.GenericType _ ->
            []

        TypeAnnotation.Typed call types ->
            visitCall call
                ++ visitTypeAnnotationList types

        TypeAnnotation.Unit ->
            []

        TypeAnnotation.Tupled list ->
            visitTypeAnnotationList list

        TypeAnnotation.Record list ->
            visitRecordFieldList list

        TypeAnnotation.GenericRecord _ list ->
            visitRecordFieldList (Node.value list)

        TypeAnnotation.FunctionTypeAnnotation argument return ->
            visitTypeAnnotation argument
                ++ visitTypeAnnotation return


visitRecordFieldList : List (Node TypeAnnotation.RecordField) -> List (Node ( ModuleName, String ))
visitRecordFieldList list =
    List.concatMap visitRecordField list


visitRecordField : Node TypeAnnotation.RecordField -> List (Node ( ModuleName, String ))
visitRecordField node =
    visitTypeAnnotation (node |> Node.value |> Tuple.second)


visitExpression : Node Expression -> List (Node ( ModuleName, String ))
visitExpression node =
    case Node.value node of
        Expression.FunctionOrValue moduleName function ->
            [ newCall moduleName function (Node.range node) ]

        Expression.LetExpression { declarations } ->
            visitLetDeclarationList declarations

        Expression.CaseExpression { cases } ->
            visitCaseList cases

        Expression.LambdaExpression { args } ->
            visitPatternList args

        _ ->
            []


visitLetDeclarationList : List (Node Expression.LetDeclaration) -> List (Node ( ModuleName, String ))
visitLetDeclarationList list =
    List.concatMap visitLetDeclaration list


visitLetDeclaration : Node Expression.LetDeclaration -> List (Node ( ModuleName, String ))
visitLetDeclaration node =
    case Node.value node of
        Expression.LetFunction { signature, declaration } ->
            visitMaybeSignature signature
                ++ visitFunctionImplementation declaration

        Expression.LetDestructuring pattern _ ->
            visitPattern pattern


visitCaseList : List Expression.Case -> List (Node ( ModuleName, String ))
visitCaseList list =
    List.concatMap visitCase list


visitCase : Expression.Case -> List (Node ( ModuleName, String ))
visitCase ( pattern, _ ) =
    visitPattern pattern


visitPatternList : List (Node Pattern) -> List (Node ( ModuleName, String ))
visitPatternList list =
    List.concatMap visitPattern list


visitPattern : Node Pattern -> List (Node ( ModuleName, String ))
visitPattern node =
    case Node.value node of
        Pattern.TuplePattern patterns ->
            visitPatternList patterns

        Pattern.UnConsPattern head rest ->
            visitPattern head ++ visitPattern rest

        Pattern.ListPattern list ->
            visitPatternList list

        Pattern.NamedPattern { moduleName, name } _ ->
            let
                { start } =
                    Node.range node

                newEnd =
                    { start | column = start.column + (name :: moduleName |> String.join "." |> String.length) }

                range =
                    { start = start, end = newEnd }
            in
            [ newCall moduleName name range ]

        Pattern.AsPattern pattern _ ->
            visitPattern pattern

        Pattern.ParenthesizedPattern pattern ->
            visitPattern pattern

        _ ->
            []


visitCall : Node ( ModuleName, String ) -> List (Node ( ModuleName, String ))
visitCall node =
    [ node ]


newCall : ModuleName -> String -> Range -> Node ( ModuleName, String )
newCall moduleName name range =
    Node range ( moduleName, name )
