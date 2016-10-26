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
    , score : Score
    }


type alias Model =
    List Round


model : Model
model =
    [ (Round [ Pink, Yellow, Blue, Green ] ( 0, 1 ))
    , (Round [ Blue, Blue, Blue, Blue ] ( 0, 0 ))
    , (Round [ Green, Red, Pink, Orange ] ( 2, 1 ))
    ]



-- UPDATE


type Msg
    = NoOp
    | Guess (List Peg)
    | SubmitScore Score
    | ChangeScore Score


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Guess pegs ->
            model

        SubmitScore newScore ->
            model ++ [ (Round [ Yellow, Yellow, Blue, Green ] ( 1, 1 )) ]

        ChangeScore newScore ->
            let
                oldRound =
                    List.getAt ((List.length model) - 1) model |> Maybe.withDefault (Round [] ( 1, 0 ))

                newRound =
                    { oldRound | score = newScore }
            in
                List.setAt ((List.length model) - 1) newRound model |> Maybe.withDefault [ (Round [] ( 0, 0 )) ]



-- this is backwards


view : Model -> Html Msg
view rounds =
    case rounds of
        -- [ x :: [] ] ->
        --     ol [ class "board" ] <| [ currentRound x ]
        [] ->
            ol [ class "board" ] []

        current :: others ->
            ol [ class "board" ] <| (currentRound current :: List.map round others)


currentRound : Round -> Html Msg
currentRound { guess, score } =
    let
        toScore default newVal =
            Result.withDefault default <| String.toInt newVal
    in
        li [ class "round" ]
            [ span [ class "pegs" ] <| List.map pegRenderer guess
            , span [ class "score" ]
                [ input
                    [ class "round__score--black"
                    , value <| toString <| fst score
                    , onInput (\x -> ChangeScore ( toScore (fst score) x, snd score ))
                    ]
                    []
                , input
                    [ class "round__score--white"
                    , value <| toString <| snd score
                    , onInput (\x -> ChangeScore ( fst score, toScore (snd score) x ))
                    ]
                    []
                ]
            ]


round : Round -> Html Msg
round { guess, score } =
    let
        show ( blackPegs, whitePegs ) =
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
