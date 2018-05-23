module Example exposing (..)

import Html.Events exposing (onInput)
import Main
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Html.Event as Event
import Test exposing (..)


type alias TestProgram model msg =
    { model : model
    , view : model -> Html msg
    , update : msg -> model -> model
    , currentModel : model
    , fromHtml : Query.Single msg
    }


simulateClick : TestProgram model msg -> String -> TestProgram model msg
simulateClick program testId =
    let
        event =
            (program.fromHtml
                |> Query.find [ Selector.attribute <| attribute "data-test-id" testId ]
                |> (Event.simulate (Event.click)))
    in
        case Event.toResult event of
            Err _ ->
                program
            Ok msg ->
                let
                    newModel = program.update msg program.model
                in
                    { program | model = newModel, fromHtml = program.view newModel |> Query.fromHtml }


testBeginnerProgram
  : { model : model
    , view : model -> Html msg
    , update : msg -> model -> model
    }
  -> TestProgram model msg
testBeginnerProgram {model, view, update} =
    TestProgram model view update model (Query.fromHtml <| view model)


type Msg = Change String | NotDo

suite : Test
suite =
    describe "Testing counter"
        [ test "(Behavioural way) Should increase the counter when click" <|
            \() ->
                let
                    program = testBeginnerProgram
                        { model = Main.model
                        , view = Main.view
                        , update = Main.update
                        }
                    programAfterFirstClick = simulateClick program "increase"
                    programAfterSecondClick = simulateClick programAfterFirstClick "increase"
                in
                    programAfterSecondClick.fromHtml
                        |> Query.find [ Selector.attribute <| attribute "data-test-id" "counter" ]
                        |> Query.has [ Selector.text "2" ]
        , test "(Common way) Should increase the counter when click" <|
                      \() ->
                          Main.view 0
                              |> Query.fromHtml
                              |> Query.find [ Selector.attribute <| attribute "data-test-id" "increase" ]
                              |> Event.simulate (Event.click)
                              |> Event.expect (Main.Increment)
        ]

