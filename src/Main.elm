port module Main exposing (..)

import API
import Array exposing (Array)
import Browser
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Ev
import Json.Decode
import List.Extra
import Task
import Words


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Letter
    = NotInWord String
    | InWord String
    | InPosition String
    | Empty String


type alias Model =
    { include : String
    , exclude : String
    , potentialAnswers : List String
    , positions : String
    , typedCharacters : Array Letter
    }


type alias Position =
    Int


type Msg
    = NoOp
    | ComputePotentialAnswers
    | ReceiveCharacter String
    | ReceiveBackspace ()
    | ToggleKey Position


init _ =
    ( { include = ""
      , exclude = ""
      , potentialAnswers = []
      , positions = ""
      , typedCharacters = Array.empty
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    H.div
        [ Attr.class "grid grid-cols-2 gap-10 p-4"
        ]
        [ H.div []
            [ H.div
                [ Attr.class "grid grid-cols-5 gap-4 w-sm max-w-sm"
                ]
                (List.indexedMap viewLetter <| Array.toList (padEmpty model.typedCharacters))
            ]
        , H.div
            []
            (List.map (\wrd -> H.div [] [ H.text wrd ]) model.potentialAnswers)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ComputePotentialAnswers ->
            let
                excludes =
                    computeExcludesFromModel model.typedCharacters

                includes =
                    computeIncludesFromModel model.typedCharacters

                positions =
                    computePositionsFromModel model.typedCharacters
            in
            ( { model
                | potentialAnswers =
                    if
                        String.isEmpty excludes
                            && String.isEmpty includes
                            && String.isEmpty positions
                    then
                        []

                    else
                        Words.words
                            |> API.exclude excludes
                            |> API.filterByLetters includes
                            |> API.filterByPos (String.split "," positions)
              }
            , Cmd.none
            )

        ReceiveCharacter str ->
            ( { model
                | typedCharacters = Array.push (NotInWord str) model.typedCharacters
              }
            , msgToCmd ComputePotentialAnswers
            )

        ReceiveBackspace _ ->
            ( { model
                | typedCharacters = Array.slice 0 -1 model.typedCharacters
              }
            , msgToCmd ComputePotentialAnswers
            )

        ToggleKey pos ->
            let
                itemAtPos =
                    Array.get pos model.typedCharacters

                replaceItem =
                    case itemAtPos of
                        Just item ->
                            case item of
                                NotInWord str ->
                                    InWord str

                                InWord str ->
                                    InPosition str

                                InPosition str ->
                                    NotInWord str

                                _ ->
                                    item

                        -- this should never happen. but have to account for all cases.
                        Nothing ->
                            Empty ""

                updatedTypedChars =
                    Array.set pos replaceItem model.typedCharacters
            in
            ( { model | typedCharacters = updatedTypedChars }, msgToCmd ComputePotentialAnswers )


msgToCmd : Msg -> Cmd Msg
msgToCmd msg =
    Task.perform (\_ -> msg) (Task.succeed True)


viewLetter : Int -> Letter -> H.Html Msg
viewLetter idx letter =
    let
        renderString : String -> String -> H.Html Msg
        renderString classes str =
            H.div
                [ Attr.class classes
                , Attr.class "inline-block flex items-center justify-center h-16 w-16 cursor-default"
                , Attr.id <| "key-" ++ String.fromInt idx
                , Ev.preventDefaultOn "click" (Json.Decode.succeed ( ToggleKey idx, True ))
                ]
                [ H.text <| String.toUpper str ]

        renderNotInWord : String -> H.Html Msg
        renderNotInWord =
            renderString "bg-gray-600 text-white"

        renderInWord : String -> H.Html Msg
        renderInWord =
            renderString "bg-yellow-400 text-white"

        renderInPosition : String -> H.Html Msg
        renderInPosition =
            renderString "bg-green-500 text-white"

        renderEmpty : String -> H.Html Msg
        renderEmpty =
            renderString "border border-gray-600"
    in
    case letter of
        NotInWord str ->
            renderNotInWord str

        InWord str ->
            renderInWord str

        InPosition str ->
            renderInPosition str

        Empty str ->
            renderEmpty str


padEmpty : Array Letter -> Array Letter
padEmpty list =
    let
        mod =
            modBy 5 (Array.length list)

        diff =
            5 - mod
    in
    Array.append list (Array.repeat diff (Empty ""))


port receiveCharacter : (String -> msg) -> Sub msg


port receiveBackspace : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveCharacter ReceiveCharacter
        , receiveBackspace ReceiveBackspace
        ]


computeIncludesFromModel : Array Letter -> String
computeIncludesFromModel letters =
    letters
        |> Array.foldl
            (\ltr acc ->
                case ltr of
                    InWord str ->
                        Array.push str acc

                    _ ->
                        acc
            )
            Array.empty
        |> Array.toList
        |> List.Extra.unique
        |> String.concat


computeExcludesFromModel : Array Letter -> String
computeExcludesFromModel letters =
    letters
        |> Array.foldl
            (\ltr acc ->
                case ltr of
                    NotInWord str ->
                        Array.push str acc

                    _ ->
                        acc
            )
            Array.empty
        |> Array.toList
        |> List.Extra.unique
        |> List.filter
            (\str ->
                let
                    stringsInPositionList =
                        computePositionsFromModel letters
                in
                String.contains (String.toUpper str) stringsInPositionList |> not
            )
        |> String.concat


computePositionsFromModel : Array Letter -> String
computePositionsFromModel letters =
    letters
        |> Array.map
            (\ltr ->
                case ltr of
                    InPosition str ->
                        String.toUpper str

                    InWord str ->
                        str

                    _ ->
                        "-"
            )
        |> Array.toList
        |> List.Extra.greedyGroupsOf 5
        |> List.map String.concat
        |> String.join ","
