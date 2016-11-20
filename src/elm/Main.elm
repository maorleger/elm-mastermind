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


type GameOver
    = Win
    | Lose
    | None


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


gameOver : List Round -> GameOver
gameOver rounds =
    let
        winRound { guess, score } =
            Maybe.withDefault False (Maybe.map (fst >> ((==) 4)) score)
    in
        case rounds of
            [] ->
                None

            current :: others ->
                case winRound current of
                    False ->
                        if List.length rounds > 7 then
                            Lose
                        else
                            None

                    True ->
                        Win


type Msg
    = NoOp
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
                    if gameOver newModel.rounds /= None then
                        ( newModel, Cmd.none )
                    else
                        ( newModel, submitScore newModel )

            ChangeBlack blackPegs ->
                ( { model | blackPegs = validateScore <| stringToScore blackPegs }, Cmd.none )

            ChangeWhite whitePegs ->
                ( { model | whitePegs = validateScore <| stringToScore whitePegs }, Cmd.none )



-- ENCODING


pegToString : Peg -> Encode.Value
pegToString =
    Encode.string << toString


encodeRounds : List Round -> Http.Body
encodeRounds rounds =
    Encode.object
        [ ( "rounds", Encode.list <| List.map encodeRound <| rounds )
        ]
        |> Encode.encode 0
        |> Http.string


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
    let
        url =
            "http://localhost:3000/play"
    in
        Task.perform FailedRounds GotRounds (Http.post decodeRounds url <| encodeRounds rounds)



-- DECODING


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
        , (scoreRenderer blackPegs whitePegs rounds)
        ]


scoreRenderer : Maybe Int -> Maybe Int -> List Round -> Html Msg
scoreRenderer blackPegs whitePegs rounds =
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
        case gameOver rounds of
            Win ->
                div [ class "results" ] [ text "I won!" ]

            Lose ->
                div [ class "results" ] [ text "Wow I suck" ]

            None ->
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
