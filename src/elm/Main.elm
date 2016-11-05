module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import String exposing (toLower)
import List.Extra as List


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
    , score : Maybe Score
    }


type alias Model =
    List Round


model : Model
model =
    [ (Round [ Pink, Yellow, Blue, Green ] <| Just ( 0, 1 ))
    , (Round [ Blue, Blue, Blue, Blue ] <| Just ( 0, 0 ))
    , (Round [ Green, Red, Pink, Orange ] <| Just ( 2, 1 ))
    , (Round [ Green, Red, Red, Red ] <| Nothing)
    ]



-- UPDATE


type Msg
    = NoOp
    | Guess (List Peg)
    | SubmitScore (Maybe Score)
    | ChangeScore (Maybe Score)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Guess pegs ->
            model

        SubmitScore newScore ->
            model

        ChangeScore newScore ->
            model


view : Model -> Html Msg
view rounds =
    ol [ class "board" ] <| List.map round rounds


round : Round -> Html Msg
round { guess, score } =
    let
        show score =
            case score of
                Nothing ->
                    span []
                        [ input [ class "round__score--black", value <| "0" ] []
                        , input [ class "round__score--white", value <| "1" ] []
                        ]

                Just ( blackPegs, whitePegs ) ->
                    span [] [ text <| "(" ++ (toString blackPegs) ++ "," ++ (toString whitePegs) ++ ")" ]
    in
        li [ class "round" ]
            [ span [ class "pegs" ] <| List.map pegRenderer guess
            , span
                [ class "score"
                , onClick <| SubmitScore score
                ]
                [ show score ]
            ]


pegRenderer : Peg -> Html Msg
pegRenderer peg =
    let
        pegColor =
            "round__peg--" ++ (toLower <| toString peg)
    in
        span [ class <| "round__peg " ++ pegColor ] []
