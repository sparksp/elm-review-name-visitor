module NameVisitorTests exposing (all)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Fuzz
import NameVisitor
import Review.Rule as Rule exposing (Error, Rule)
import Review.Test
import Test exposing (Test, describe, fuzz, test)


all : Test
all =
    describe "NameVisitor"
        [ contextTests
        , declarationTests
        , expressionTests
        , patternTests
        , typeAnnotationTests
        , visitorTests
        ]


declarationTests : Test
declarationTests =
    describe "Declaration"
        [ fuzz Fuzz.string "FunctionDeclaration" <|
            \context ->
                """
module Page exposing (view)
view : Node (Html.Attribute msg) -> Html msg
view (Node attribute) = Html.div [ attribute ] []
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Node"
                            |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                        , expectedError context "Html.Attribute"
                        , expectedError context "Html"
                            |> Review.Test.atExactly { start = { row = 3, column = 37 }, end = { row = 3, column = 41 } }
                        , expectedError context "Node"
                            |> Review.Test.atExactly { start = { row = 4, column = 7 }, end = { row = 4, column = 11 } }
                        ]
        , fuzz Fuzz.string "AliasDeclaration" <|
            \context ->
                """
module Page exposing (Page)
type alias Page =
    { title : String
    , body : List (Html msg)
    }
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "String"
                        , expectedError context "List"
                        , expectedError context "Html"
                        ]
        , fuzz Fuzz.string "CustomTypeDeclaration" <|
            \context ->
                """
module Page exposing (Page)
type Page
    = Home Route.Home
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Route.Home"
                        ]
        , fuzz Fuzz.string "PortDeclaration" <|
            \context ->
                """
port module Ports exposing (alarm)
port alarm : Json.Encode.Value -> Cmd msg
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Json.Encode.Value"
                        , expectedError context "Cmd"
                        ]
        ]


expressionTests : Test
expressionTests =
    describe "Expression"
        [ fuzz Fuzz.string "FunctionOrValue" <|
            \context ->
                """
module Page exposing (view)
view = Html.div [] []
"""
                    |> Review.Test.run (expressionVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Html.div"
                        ]
        , fuzz Fuzz.string "LetDestructuring" <|
            \context ->
                """
module Page exposing (view)
view =
    let
        html = Html.span [] []
    in
    html
"""
                    |> Review.Test.run (expressionVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Html.span"
                        , expectedError context "html"
                            |> Review.Test.atExactly { start = { row = 7, column = 5 }, end = { row = 7, column = 9 } }
                        ]
        , fuzz Fuzz.string "LetFunction" <|
            \context ->
                """
module Page exposing (view)
view =
    let
        html : Nested.Html msg
        html (Nested.Node name) = span [] []
    in
    html
"""
                    |> Review.Test.run (expressionVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Nested.Html"
                        , expectedError context "Nested.Node"
                        , expectedError context "span"
                        , expectedError context "html"
                            |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 9 } }
                        ]
        , fuzz Fuzz.string "CaseExpression" <|
            \context ->
                """
module Rule exposing (visitor)
visitor node =
    case Node.value node of
        Expression.FunctionOrValue _ _ ->
            []
"""
                    |> Review.Test.run (expressionVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Node.value"
                        , expectedError context "node"
                            |> Review.Test.atExactly { start = { row = 4, column = 21 }, end = { row = 4, column = 25 } }
                        , expectedError context "Expression.FunctionOrValue"
                        ]
        , fuzz Fuzz.string "LambdaExpression" <|
            \context ->
                """
module Page exposing (view)
view nodes =
    Html.div [] (List.map (\\(Node name) -> Html.text name))
"""
                    |> Review.Test.run (expressionVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Html.div"
                        , expectedError context "List.map"
                        , expectedError context "Node"
                        , expectedError context "Html.text"
                        , expectedError context "name"
                            |> Review.Test.atExactly { start = { row = 4, column = 54 }, end = { row = 4, column = 58 } }
                        ]
        ]


patternTests : Test
patternTests =
    describe "Pattern"
        [ fuzz Fuzz.string "TuplePattern" <|
            \context ->
                """
module Page exposing (view)
view ( Nested.Node name, Nested.Value value ) =
    Html.div [] [ Html.text name, Html.text value ]
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Nested.Node"
                        , expectedError context "Nested.Value"
                        ]
        , fuzz Fuzz.string "UnConsPattern" <|
            \context ->
                """
module Page exposing (list)
list children =
    case children of
        (Page.First first) :: (Page.Second second) :: _ ->
            Html.text ""
"""
                    |> Review.Test.run (expressionVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "children"
                            |> Review.Test.atExactly { start = { row = 4, column = 10 }, end = { row = 4, column = 18 } }
                        , expectedError context "Page.First"
                        , expectedError context "Page.Second"
                        , expectedError context "Html.text"
                        ]
        , fuzz Fuzz.string "ListPattern" <|
            \context ->
                """
module Page exposing (list)
list children =
    case children of
        [ Page.First first, Page.Second second ] ->
            Html.text ""
"""
                    |> Review.Test.run (expressionVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "children"
                            |> Review.Test.atExactly { start = { row = 4, column = 10 }, end = { row = 4, column = 18 } }
                        , expectedError context "Page.First"
                        , expectedError context "Page.Second"
                        , expectedError context "Html.text"
                        ]
        , fuzz Fuzz.string "NamedPattern" <|
            \context ->
                """
module Page exposing (view)
view (Node name) = Html.text name
"""
                    |> Review.Test.run (expressionVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Html.text"
                        , expectedError context "name"
                            |> Review.Test.atExactly { start = { row = 3, column = 30 }, end = { row = 3, column = 34 } }
                        ]
        , fuzz Fuzz.string "AsPattern" <|
            \context ->
                """
module Page exposing (view)
view ((Page.Document title body) as doc)
    = Html.text title
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Page.Document"
                        ]
        , fuzz Fuzz.string "ParenthesizedPattern" <|
            \context ->
                """
module Page exposing (view)
view (Nested.Node name) = Html.text name
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Nested.Node"
                        ]
        ]


typeAnnotationTests : Test
typeAnnotationTests =
    describe "TypeAnnotation"
        [ fuzz Fuzz.string "Types" <|
            \context ->
                """
module Page exposing (view)
view : Nested.Node Page.Document msg -> Html msg
view node = div [] []
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Nested.Node"
                        , expectedError context "Page.Document"
                        , expectedError context "Html"
                        ]
        , fuzz Fuzz.string "Tupled" <|
            \context ->
                """
module Page exposing (Page)
type Page
    = Page ( Document.Title, Document.List Document.Item )
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Document.Title"
                        , expectedError context "Document.List"
                        , expectedError context "Document.Item"
                        ]
        , fuzz Fuzz.string "Record" <|
            \context ->
                """
module Page exposing (Page)
type Page
    = Page { title : Document.Title, body : Document.List Document.Item }
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Document.Title"
                        , expectedError context "Document.List"
                        , expectedError context "Document.Item"
                        ]
        , fuzz Fuzz.string "GenericRecord" <|
            \context ->
                """
module Page exposing (Page)
type Page
    = Page { page | title : Document.Title, body : Document.List Document.Item }
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Document.Title"
                        , expectedError context "Document.List"
                        , expectedError context "Document.Item"
                        ]
        , fuzz Fuzz.string "FunctionTypeAnnotation" <|
            \context ->
                """
module Page exposing (Viewer)
type Viewer msg
    = Viewer (Document.Title -> Document.List Document.Item -> Html msg)
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Document.Title"
                        , expectedError context "Document.List"
                        , expectedError context "Document.Item"
                        , expectedError context "Html"
                        ]
        ]


visitorTests : Test
visitorTests =
    describe "Visitors"
        [ fuzz Fuzz.string "withNameVisitor" <|
            \context ->
                """
module Page exposing (view)
view : Page -> Html msg
view page =
    Html.div [] (Page.body page)
"""
                    |> Review.Test.run (nameVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Page"
                            |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                        , expectedError context "Html"
                            |> Review.Test.atExactly { start = { row = 3, column = 16 }, end = { row = 3, column = 20 } }
                        , expectedError context "Html.div"
                        , expectedError context "Page.body"
                        , expectedError context "page"
                            |> Review.Test.atExactly { start = { row = 5, column = 28 }, end = { row = 5, column = 32 } }
                        ]
        , fuzz Fuzz.string "withDeclarationListVisitor" <|
            \context ->
                """
module Page exposing (view)
view : Page -> Html msg
view page =
    Html.div [] (Page.body page)
"""
                    |> Review.Test.run (declarationListVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Page"
                            |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                        , expectedError context "Html"
                            |> Review.Test.atExactly { start = { row = 3, column = 16 }, end = { row = 3, column = 20 } }
                        ]
        , fuzz Fuzz.string "withExpressionVisitor" <|
            \context ->
                """
module Page exposing (view)
view : Page -> Html msg
view page =
    Html.div [] (Page.body page)
"""
                    |> Review.Test.run (expressionVisitorRule context)
                    |> Review.Test.expectErrors
                        [ expectedError context "Html.div"
                        , expectedError context "Page.body"
                        , expectedError context "page"
                            |> Review.Test.atExactly { start = { row = 5, column = 28 }, end = { row = 5, column = 32 } }
                        ]
        ]


contextTests : Test
contextTests =
    describe "Context"
        [ test "mutates context" <|
            \_ ->
                """
module Page exposing (Page)
view : Page -> Html msg
view page =
    Html.div [] (Page.body page)
"""
                    |> Review.Test.run (countingVisitorRule 0)
                    |> Review.Test.expectErrors
                        [ expectedError "0" "Page"
                            |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 12 } }
                        , expectedError "1" "Html"
                            |> Review.Test.atExactly { start = { row = 3, column = 16 }, end = { row = 3, column = 20 } }
                        , expectedError "2" "Html.div"
                        , expectedError "3" "Page.body"
                        , expectedError "4" "page"
                            |> Review.Test.atExactly { start = { row = 5, column = 28 }, end = { row = 5, column = 32 } }
                        ]
        ]



-- TEST HELPERS


expectedError : String -> String -> Review.Test.ExpectedError
expectedError context name =
    Review.Test.error
        { message = "Test Error for `" ++ name ++ "`."
        , details = [ "The context of this error was:", context ]
        , under = name
        }



-- RULE HELPERS


nameVisitorRule : String -> Rule
nameVisitorRule context =
    Rule.newModuleRuleSchema "NameVisitor" context
        |> NameVisitor.withNameVisitor nameVisitor
        |> Rule.fromModuleRuleSchema


declarationListVisitorRule : String -> Rule
declarationListVisitorRule context =
    Rule.newModuleRuleSchema "DeclarationListVisitor" context
        |> Rule.withDeclarationListVisitor (NameVisitor.declarationListVisitor nameVisitor)
        |> Rule.fromModuleRuleSchema


expressionVisitorRule : String -> Rule
expressionVisitorRule context =
    Rule.newModuleRuleSchema "ExpressionVisitor" context
        |> Rule.withExpressionVisitor (NameVisitor.expressionVisitor nameVisitor)
        |> Rule.fromModuleRuleSchema


nameVisitor : Node ( ModuleName, String ) -> String -> ( List (Error {}), String )
nameVisitor node context =
    ( [ simpleError context node ], context )


countingVisitorRule : Int -> Rule
countingVisitorRule counter =
    Rule.newModuleRuleSchema "NameVisitor" counter
        |> NameVisitor.withNameVisitor countingVisitor
        |> Rule.fromModuleRuleSchema


countingVisitor : Node ( ModuleName, String ) -> Int -> ( List (Error {}), Int )
countingVisitor node counter =
    ( [ simpleError (String.fromInt counter) node ], counter + 1 )


simpleError : String -> Node ( ModuleName, String ) -> Error {}
simpleError context node =
    Rule.error
        { message = "Test Error for `" ++ formatNode node ++ "`."
        , details = [ "The context of this error was:", context ]
        }
        (Node.range node)


formatNode : Node ( ModuleName, String ) -> String
formatNode node =
    node
        |> Node.value
        |> formatTuple


formatTuple : ( ModuleName, String ) -> String
formatTuple ( moduleName, name ) =
    String.join "." (moduleName ++ [ name ])
