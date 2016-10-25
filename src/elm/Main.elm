module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)
import String exposing (toLower)


-- APP


main : Program Never
main =
    Html.beginnerProgram { model = model, view = view, update = update }


type Peg
    = Orange
    | Yellow
    | Green
    | Red
    | Blue
    | Pink


type alias Score =
    ( Int, Int )



-- MODEL


type alias Round =
    { guess : List Peg
    , score : Score
    }


type alias Model =
    List Round


model : Model
model =
    [ (Round [ Pink, Yellow, Blue, Green ] ( 0, 1 ))
    , (Round [ Blue, Blue, Blue, Blue ] ( 0, 0 ))
    ]



-- UPDATE


type Msg
    = NoOp
    | Guess (List Peg)
    | SubmitScore Score


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Guess pegs ->
            model

        SubmitScore newScore ->
            model ++ [ (Round [ Yellow, Yellow, Blue, Green ] ( 0, 1 )) ]


view : Model -> Html Msg
view model =
    ol [ class "board" ] <| List.map round model


round : Round -> Html Msg
round { guess, score } =
    li [ class "round" ]
        [ span [ class "pegs" ] <| List.map pegRenderer guess
        , span
            [ class "score"
            , onClick <| SubmitScore score
            ]
            [ text ("score") ]
        ]


pegRenderer : Peg -> Html Msg
pegRenderer peg =
    let
        pegColor =
            "round__peg--" ++ (toLower <| toString peg)
    in
        span [ class <| "round__peg " ++ pegColor ] []
