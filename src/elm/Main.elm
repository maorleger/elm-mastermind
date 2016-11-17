module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import String exposing (toLower)


-- APP


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Model
        [ (Round [ Green, Red, Red, Red ] <| Nothing)
        , (Round [ Blue, Blue, Blue, Blue ] <| Just ( 0, 0 ))
        , (Round [ Green, Red, Pink, Orange ] <| Just ( 2, 1 ))
        , (Round [ Pink, Yellow, Blue, Green ] <| Just ( 0, 1 ))
        ]
        (Just 1)
        Nothing
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = NoOp
    | Guess (List Peg)
    | SubmitScore ( Maybe Int, Maybe Int )
    | ChangeBlack String
    | ChangeWhite String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        stringToScore =
            Result.toMaybe << String.toInt

        -- TODO: better error messages if model.blackPegs and model.whitePegs dont exist yet
        -- TODO: refactor all the validation to some applicative
        updateRound { guess, score } =
            case score of
                Nothing ->
                    Round guess <| Just ( Maybe.withDefault 0 model.blackPegs, Maybe.withDefault 0 model.whitePegs )

                Just score ->
                    Round guess <| Just score

        validateScore score =
            case score of
                Nothing ->
                    Nothing

                Just score ->
                    if List.member score [ 0, 1, 2, 3, 4 ] then
                        Just score
                    else
                        Nothing
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            Guess pegs ->
                ( model, Cmd.none )

            SubmitScore ( blackPegs, whitePegs ) ->
                ( Model ((Round [ Pink, Pink, Pink, Pink ] Nothing) :: (List.map updateRound model.rounds)) Nothing Nothing, Cmd.none )

            ChangeBlack blackPegs ->
                ( { model | blackPegs = validateScore <| stringToScore blackPegs }, Cmd.none )

            ChangeWhite whitePegs ->
                ( { model | whitePegs = validateScore <| stringToScore whitePegs }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { rounds, blackPegs, whitePegs } =
    div [ class "board" ]
        [ (div [ class "rounds" ] <| List.map round rounds)
        , (scoreRenderer blackPegs whitePegs)
        ]


scoreRenderer : Maybe Int -> Maybe Int -> Html Msg
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
        div [ class "score form-inline" ]
            [ div [ class "form-group" ]
                [ label [ for "black-pegs" ] [ text "Black Pegs:" ]
                , input [ class "score__input--black", id "black-pegs", value <| parseScore blackPegs, onInput ChangeBlack ] []
                ]
            , div [ class "form-group" ]
                [ label [ for "white-pegs" ] [ text "White Pegs:" ]
                , input [ class "score__input--white", id "white-pegs", value <| parseScore whitePegs, onInput ChangeWhite ] []
                ]
            , button [ class "score__button--submit btn btn-primary", onClick <| SubmitScore ( blackPegs, whitePegs ) ] [ text "Submit" ]
            ]


round : Round -> Html Msg
round { guess, score } =
    let
        show score =
            case score of
                Nothing ->
                    "[ , ]"

                Just ( blackPegs, whitePegs ) ->
                    "[" ++ (toString blackPegs) ++ "," ++ (toString whitePegs) ++ "]"
    in
        div [ class "round" ]
            [ div [ class "round__pegs" ] <| List.map pegRenderer guess
            , div
                [ class "round__score" ]
                [ text <| show score ]
            ]


pegRenderer : Peg -> Html Msg
pegRenderer peg =
    let
        pegColor =
            "round__peg--" ++ (toLower <| toString peg)
    in
        span [ class <| "round__peg " ++ pegColor ] []
