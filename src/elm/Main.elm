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
    { rounds : List Round, blackPegs : Maybe Int, whitePegs : Maybe Int }


model : Model
model =
    Model
        [ (Round [ Pink, Yellow, Blue, Green ] <| Just ( 0, 1 ))
        , (Round [ Blue, Blue, Blue, Blue ] <| Just ( 0, 0 ))
        , (Round [ Green, Red, Pink, Orange ] <| Just ( 2, 1 ))
        , (Round [ Green, Red, Red, Red ] <| Nothing)
        ]
        (Just 1)
        Nothing



-- UPDATE


type Msg
    = NoOp
    | Guess (List Peg)
    | SubmitScore ( Maybe Int, Maybe Int )
    | ChangeBlack String
    | ChangeWhite String


update : Msg -> Model -> Model
update msg model =
    let
        stringToScore =
            Result.toMaybe << String.toInt
    in
        case msg of
            NoOp ->
                model

            Guess pegs ->
                model

            SubmitScore ( blackPegs, whitePegs ) ->
                model

            ChangeBlack blackPegs ->
                { model | blackPegs = stringToScore blackPegs }

            ChangeWhite whitePegs ->
                { model | whitePegs = stringToScore whitePegs }


view : Model -> Html Msg
view { rounds, blackPegs, whitePegs } =
    div [ class "board" ] <| List.map round rounds ++ (scoreRenderer blackPegs whitePegs)


scoreRenderer : Maybe Int -> Maybe Int -> List (Html Msg)
scoreRenderer blackPegs whitePegs =
    let
        parseScore score =
            case score of
                Nothing ->
                    ""

                Just s ->
                    toString s

        toScore blackPegs whitePegs =
            case String.isEmpty <| blackPegs ++ whitePegs of
                True ->
                    Nothing

                False ->
                    Just ( blackPegs, whitePegs )
    in
        [ div [ class "board--score" ]
            [ input [ class "round__score--black", value <| parseScore blackPegs, onInput ChangeBlack ] []
            , input [ class "round__score--white", value <| parseScore whitePegs, onInput ChangeWhite ] []
            , button [ class "round__score--submit", onClick <| SubmitScore ( blackPegs, whitePegs ) ] [ text "Submit" ]
            ]
        ]


round : Round -> Html Msg
round { guess, score } =
    let
        show score =
            case score of
                Nothing ->
                    span [] []

                Just ( blackPegs, whitePegs ) ->
                    span [] [ text <| "(" ++ (toString blackPegs) ++ "," ++ (toString whitePegs) ++ ")" ]
    in
        div [ class "round" ]
            [ span [ class "pegs" ] <| List.map pegRenderer guess
            , span
                [ class "score" ]
                [ show score ]
            ]


pegRenderer : Peg -> Html Msg
pegRenderer peg =
    let
        pegColor =
            "round__peg--" ++ (toLower <| toString peg)
    in
        span [ class <| "round__peg " ++ pegColor ] []
