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
import Models exposing (..)


-- APP


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { rounds : List Round, blackPegs : Maybe Int, whitePegs : Maybe Int, gameOver : GameOver }



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model [] Nothing Nothing None
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

        loseRound =
            if List.length rounds > 7 then
                Lose
            else
                None

        lastRound =
            Maybe.withDefault (Round [] Nothing) (List.head rounds)
    in
        if winRound lastRound then
            Win
        else
            loseRound


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
                ( Model rounds Nothing Nothing None, Cmd.none )

            FailedRounds error ->
                ( { model | gameOver = Error error }, Cmd.none )

            SubmitScore ( blackPegs, whitePegs ) ->
                let
                    updatedRounds =
                        case model.rounds of
                            [] ->
                                []

                            current :: others ->
                                (updateRound current) :: others

                    newModel =
                        Model updatedRounds Nothing Nothing (gameOver updatedRounds)
                in
                    if newModel.gameOver /= None then
                        ( newModel, Cmd.none )
                    else
                        ( newModel, submitScore newModel )

            ChangeBlack blackPegs ->
                ( { model | blackPegs = validateScore <| stringToScore blackPegs }, Cmd.none )

            ChangeWhite whitePegs ->
                ( { model | whitePegs = validateScore <| stringToScore whitePegs }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { rounds, blackPegs, whitePegs, gameOver } =
    div [ class "board" ]
        [ (div [ class "rounds" ] <| List.map roundView rounds)
        , (scoreView blackPegs whitePegs gameOver)
        ]


scoreView : Maybe Int -> Maybe Int -> GameOver -> Html Msg
scoreView blackPegs whitePegs gameOver =
    case gameOver of
        Win ->
            div [ class "results" ] [ text "Ha! And they say computers are dumb..." ]

        Lose ->
            div [ class "results" ] [ text "Dang it! I'm just a dumb computer after all..." ]

        Error error ->
            errorView error

        None ->
            scoreFormView blackPegs whitePegs


scoreFormView : Maybe Int -> Maybe Int -> Html Msg
scoreFormView blackPegs whitePegs =
    let
        parseScore score =
            case score of
                Nothing ->
                    ""

                Just s ->
                    toString s
    in
        div [ class "form" ]
            [ div [ class "score" ]
                [ div [ class "score__input" ]
                    [ label [ for "black-pegs" ] [ text "Black:" ]
                    , input [ class "score__input--black", id "black-pegs", value <| parseScore blackPegs, onInput ChangeBlack ] []
                    ]
                , div [ class "score__input" ]
                    [ label [ for "white-pegs" ] [ text "White:" ]
                    , input [ class "score__input--white", id "white-pegs", value <| parseScore whitePegs, onInput ChangeWhite ] []
                    ]
                ]
            , div [ class "score__submit" ]
                [ button [ class "score__submit--button", onClick <| SubmitScore ( blackPegs, whitePegs ) ] [ text "Score!" ]
                ]
            ]


errorView : Http.Error -> Html Msg
errorView error =
    let
        errorText =
            case error of
                Http.Timeout ->
                    "Timeout connecting to server..."

                Http.NetworkError ->
                    "Network error connecting to server..."

                Http.UnexpectedPayload payload ->
                    "Hmmm... Got a parse error. Weird..."

                Http.BadResponse code response ->
                    "Hmmm... Could not deduce the next guess. Are you sure you scored my guesses correctly?"
    in
        div
            [ class "results" ]
            [ text errorText ]


roundView : Round -> Html Msg
roundView { guess, score } =
    let
        show =
            Maybe.map
                (\( blackPegs, whitePegs ) ->
                    "[" ++ (toString blackPegs) ++ "," ++ (toString whitePegs) ++ "]"
                )
                >> Maybe.withDefault "[ , ]"
    in
        div [ class "round" ]
            [ div [ class "round__pegs" ] <| List.map pegView guess
            , div
                [ class "round__score" ]
                [ text <| show score ]
            ]


pegView : Peg -> Html Msg
pegView peg =
    let
        pegColor =
            "round__peg--" ++ (toLower <| toString peg)
    in
        span [ class <| "round__peg " ++ pegColor ] []



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
