module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import String exposing (toLower)
import Http
import Json.Decode as Decode exposing ((:=))
import Json.Decode.Extra as Decode exposing ((|:))
import Task
import Json.Encode as Encode


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
    let
        model =
            Model [] Nothing Nothing
    in
        ( model
        , submitScore <| model
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = NoOp
    | GetRounds
    | GotRounds (List Round)
    | FailedRounds Http.Error
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
            let
                orDefault =
                    Maybe.withDefault 0
            in
                Round guess <| Just ( orDefault model.blackPegs, orDefault model.whitePegs )

        validateScore =
            let
                scoreInRange score =
                    if List.member score [ 0, 1, 2, 3, 4 ] then
                        Just score
                    else
                        Nothing
            in
                flip Maybe.andThen scoreInRange
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            GetRounds ->
                ( model, getRounds )

            GotRounds rounds ->
                ( Model rounds Nothing Nothing, Cmd.none )

            FailedRounds error ->
                Debug.crash <| toString error

            SubmitScore ( blackPegs, whitePegs ) ->
                let
                    updatedRounds =
                        case model.rounds of
                            [] ->
                                []

                            current :: others ->
                                (updateRound current) :: others

                    newModel =
                        Model updatedRounds Nothing Nothing
                in
                    ( newModel, submitScore newModel )

            ChangeBlack blackPegs ->
                ( { model | blackPegs = validateScore <| stringToScore blackPegs }, Cmd.none )

            ChangeWhite whitePegs ->
                ( { model | whitePegs = validateScore <| stringToScore whitePegs }, Cmd.none )



-- ENCODING


pegToString : Peg -> Encode.Value
pegToString =
    Encode.string << toString



--
-- encodeRounds : List Round -> List Encode.Value
-- encodeRounds rounds =
--     List.map encodeRound rounds


encodeRounds : List Round -> Encode.Value
encodeRounds rounds =
    Encode.object
        [ ( "rounds", Encode.list <| List.map encodeRound <| rounds )
        ]


encodeRound : Round -> Encode.Value
encodeRound record =
    let
        toScore score =
            case score of
                Nothing ->
                    [ 0, 0 ]

                Just ( black, white ) ->
                    [ black, white ]
    in
        Encode.object
            [ ( "guess", Encode.list <| List.map pegToString <| record.guess )
            , ( "score", Encode.list <| List.map Encode.int <| toScore record.score )
            ]


submitScore : Model -> Cmd Msg
submitScore { rounds, blackPegs, whitePegs } =
    Task.perform FailedRounds GotRounds (Http.post decodeRounds "http://localhost:3000/play" <| Http.string <| Encode.encode 0 <| encodeRounds rounds)



-- DECODING


getRounds : Cmd Msg
getRounds =
    let
        url =
            "http://localhost:3000/rounds"
    in
        Task.perform FailedRounds GotRounds (Http.get decodeRounds url)


stringToPeg : String -> Decode.Decoder Peg
stringToPeg peg =
    case peg of
        "Orange" ->
            Decode.succeed Orange

        "Yellow" ->
            Decode.succeed Yellow

        "Green" ->
            Decode.succeed Green

        "Red" ->
            Decode.succeed Red

        "Blue" ->
            Decode.succeed Blue

        "Pink" ->
            Decode.succeed Pink

        _ ->
            Decode.fail "that aint a color"


decodeRounds : Decode.Decoder (List Round)
decodeRounds =
    ("rounds" := Decode.list gameRound)


gameRound : Decode.Decoder Round
gameRound =
    Decode.succeed Round
        |: ("guess" := Decode.list decodePeg)
        |: ("score" := (Decode.maybeNull <| Decode.tuple2 (,) Decode.int Decode.int))


decodePeg : Decode.Decoder Peg
decodePeg =
    Decode.andThen Decode.string stringToPeg



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
              --, button [ class "score__button--submit btn btn-primary", onClick <| GetRounds ( blackPegs, whitePegs ) ] [ text "Submit" ]
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
