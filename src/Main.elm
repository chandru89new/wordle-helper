module Main exposing (..)

import API
import Browser
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Task
import Words


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { include : String
    , exclude : String
    , potentialAnswers : List String
    , greenPositions : String
    , yellowPositions : String
    }


type Msg
    = NoOp
    | UpdateExcludes String
    | UpdateIncludes String
    | UpdateYellowPositions String
    | UpdateGreenPositions String
    | ComputePotentialAnswers


init _ =
    ( { include = ""
      , exclude = ""
      , potentialAnswers = []
      , greenPositions = ""
      , yellowPositions = ""
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    H.div [ Attr.class "flex gap-10 flex-col p-10" ]
        [ H.div []
            [ H.input
                [ Attr.value model.exclude
                , Ev.onInput UpdateExcludes
                , Attr.placeholder "Exclusions"
                , Attr.class
                    (if String.isEmpty model.exclude then
                        "empty-input"

                     else
                        ""
                    )
                ]
                []
            ]
        , H.div []
            [ H.input
                [ Attr.value model.include
                , Ev.onInput UpdateIncludes
                , Attr.placeholder "Include"
                , Attr.class
                    (if String.isEmpty model.include then
                        "empty-input"

                     else
                        ""
                    )
                ]
                []
            ]
        , H.div []
            [ H.input
                [ Attr.value model.yellowPositions
                , Ev.onInput UpdateYellowPositions
                , Attr.placeholder "Yellows"
                , Attr.class
                    (if String.isEmpty model.yellowPositions then
                        "empty-input"

                     else
                        ""
                    )
                ]
                []
            ]
        , H.div []
            [ H.input
                [ Attr.value model.greenPositions
                , Ev.onInput UpdateGreenPositions
                , Attr.placeholder "Greens"
                , Attr.class
                    (if String.isEmpty model.greenPositions then
                        "empty-input"

                     else
                        ""
                    )
                ]
                []
            ]
        , H.div [] [ H.text <| String.join ", " model.potentialAnswers ]
        ]


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateExcludes str ->
            ( { model | exclude = String.trim str }, msgToCmd ComputePotentialAnswers )

        UpdateIncludes str ->
            ( { model | include = String.trim str }, msgToCmd ComputePotentialAnswers )

        UpdateYellowPositions str ->
            ( { model | yellowPositions = String.trim str }, msgToCmd ComputePotentialAnswers )

        UpdateGreenPositions str ->
            ( { model | greenPositions = String.trim str }, msgToCmd ComputePotentialAnswers )

        ComputePotentialAnswers ->
            ( { model
                | potentialAnswers =
                    if
                        String.isEmpty model.exclude
                            && String.isEmpty model.include
                            && String.isEmpty model.greenPositions
                            && String.isEmpty model.yellowPositions
                    then
                        []

                    else
                        Words.words
                            |> API.exclude model.exclude
                            |> API.filterByLetters model.include
                            |> API.filterByPosition model.greenPositions
                            |> API.filterByPositionNotMultiple (String.split "," model.yellowPositions)
              }
            , Cmd.none
            )


msgToCmd : Msg -> Cmd Msg
msgToCmd msg =
    Task.perform (\_ -> msg) (Task.succeed True)
